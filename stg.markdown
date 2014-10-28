---
title: The Guts of a Spineless G Machine
tags: haskell
---

It's fairly well known that Haskell is a bit um.. different then how
stock hardware sees the world. As such, compiling Haskell (and indeed
most functional languages) presents an interesting challenge. How do
we go about mapping our lovely highlevel constructs to instructions
without sacrificing efficiency? To this end something called the
"spineless tagless G-machine" is employed by GHC and a few
other functional languages to compile to stock hardware. In this post
I'll go over some of the basic ideas behind STG, hopefully in the
process you'll learn a bit about how Haskell is implemented.

### Core Concepts

The basic idea behind a compiler intent on going the STG route is
something like

 1. .. front end stuff ..
 2. Translate IL to STG language
 3. Compile STG language to C/ASM/LLVM/Javascript

In GHC case I understand the pipeline is something like

 1. Parsing
 2. Typechecking
 3. Desugaring + a few bobs and bits
 4. Translation to core
 5. Lion share of optimization
 6. Translation to STG language
 7. STG language to C--
 8. C-- to assembly

We're really concerned with with parts 6 and 7 here. First things
first, let's lay out what's exactly in the STG language. It's a tiny
functional language that looks a bit like Haskell or Core, with a few
restrictions. A program is simply a series of bindings, much like
Haskell. The top levels look something like

    f = {x y z} flag {a b c} -> ...

You should read this for now as `f = \a b c -> ...`. The first set of
variables and the flag correspond to some stuff we'll discuss later.

Inside the `...` we can write most o what you would expect form
Haskell. We have let[rec] bindings, case expressions, application,
constructors, literals, and primitives. There is a caveat though,
first off all constructor applications must be fully saturated. This
isn't unlike OCaml or something where you can't just treat a
constructor as a function with an arbitrary name. We would write

    \a -> Just a

instead of just `Just`. Another bit of trickiness, our language has no
lambdas! So we can't even write the above. Instead if we had something
like

     map Just [1, 2, 3]

We'd have to write

     let f   = \a -> Just a
         l'' = 3 : nil
         l'  = 2 : l''
         l   = 1 : l'
     in map f l

The reason for the awkward `l''` series is that we're only allowed to
apply constructors and functions to atoms (literals and
variables).

One other noteworthy feature of STG is that we have primitive
operations. They need to be fully saturated, just like constructors,
but they work across unboxed things. For example there'd probably be
something like `+#` which adds to unboxed integers. To work with these
we also have unboxed literals, `1#`, `2#`, so on and so on.

No despite all these limitations placed on STG, it's still a pretty
stinking highlevel language. There's letrec, higher order functions, a
lot of the normal stuff we'd expect in a functional language. This
means it's not actually to hard to compile something like Haskell or
Core to STG (hard is relative).

As an example, let's look at translating factorial into STG
language. We start with

    f :: Int -> Int
    f i = case i of
      0 -> 1
      i -> i * (f (i - 1))

Now the first step is we change the binding form

    f = {} n {i} -> ...

The case expressions clause can remain the same, we're already casing
on an atom

    case i of
      (MkInt# i#) -> ...

Now comes the first big change, our boxed integers are going to get in
the way here, so the case expression strips away the constructor
leaving us with an unboxed integer.

     case i of
       MkInt i# -> case i# -# 1# of
           dec# ->
             let dec = \{dec#} u {} -> MkInt dec#
             in case fact dec of
             MkInt rest# -> case i# * rest# of
               result# -> MkInt result#


Now we can see what those extra {}'s were for. They notate the free
variables for a thunk. `dec` for example has a free variable `dec#`
and it exists to box that result for the recursive call to
factorial. We use `case` expressions to get evaluation. Most programs
thus become chains of `case`'s and `let` alternating between creating
thunks and actually doing work.

That `u` in between the {}'s in `dec` was also important. It's the
update flag. Remember how in Haskell we don't want to force the same
thunk twice. If I say

    let i = 1 + 1 in i + i

We should only evaluate `1 + 1` once. That means that the thunk `i`
will become has to do some updating. The update flag signifies the
difference between thunks that we want to update and thunks that we
don't. For example, if we replaced the thunk for `+` with the first
result it returned, we'd be mighty surprised. Suddenly `1 + 1 + 1` is
just 2!

The `u` flag says "yes, I'm just a normal expression that should be
updated" and the n flag says the opposite.

That about wraps up our discussion of the STG language, let's talk
about how to implement it now.

### Implementation

This language wouldn't be much good if it didn't lend itself to an
easy implementation, indeed we find that the restrictions we placed
upon the language prove to be invaluable for its compilation (almost
like they were designed that way!).

In order to decide how best to implement it, we first let at the
formal semantics for our language. We give these semantics a tuple of
6 things.

 1. The code - the instruction we're currently executing
 2. The argument stack - A stack of integers or pointers to closures
 3. The return stack - A stack of continuations
 4. The update stack - A stack of update frames and sadness
 5. The heap - A map from addresses to closures
 6. The environment - A map from names to addresses of toplevel
    closures

A code is more or less the current thign we're attempting to do. It's
either

 1. `Eval e p` - evaluate an expression in an environment
 2. `Enter a` - Enter a closure
 3. `ReturnCon c ws` - Return a constructor applied to some arguments
 4. `ReturnInt` - Return an integer

Now the idea is we're going to "unroll" our computations into pushing
things onto the continuation stack and entering closures. We start
with the code `Eval main {}`. That is to say, we start by running
`main`. Then if we're looking at a `case` we do something really
clever

     EVAL(case expr of {pat1 -> expr1; ...}, p) as rs us h o

becomes

    EVAL (expr, p) as ({pat1 -> expr1; ...} : rs) us h o

That is to say, we just push the pattern matching on to the
continuation stack and evaluate the expression.

Presumably at some point we'll get to a "leaf" in our expression, some
random literal or constructor. At this point we make use of our
continuation stack

    EVAL (C ws, p) as ((...; c vs -> expr; ...) : rs) us h o
    ReturnCon (C ws) as ((...; c vs -> expr; ...) : rs) us h o
    EVAL (expr, p[vs -> ws]) as rs us h o

So our pattern matching is rolled into `ReturnCon`. `ReturnCon` will
just look on top of the return stack looking for a continuation which
wants its constructor and evaluate its expression, mapping the
constructor's variables to the pattern's variables.

The story is similar for literals

    EVAL (Int i, p) as ((...; c vs -> expr; ...) : rs) us h o
    ReturnInt i as ((...; i -> expr; ...) : rs) us h o
    EVAL (expr, p) as rs us h o

Another phase is how we handle let's and letrec's.

This is the core idea of the spineless tagless G machine, pattern
matching is done through continuations
