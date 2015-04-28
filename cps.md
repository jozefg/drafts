---
title: Compiling With CPS
tags: compilers, haskell
---

Hello folks. It's been a busy month so I haven't had much a chance to write but
I think now's a good time to talk about another compiler related subject:
continuation passing style conversion.

When you're compiling a functional languages (in a sane way) your compiler
mostly consists of phases which run over the AST and simplify it. For example in
a language with pattern matching, it's almost certainly the case that we can
write something like

``` haskell
    case x of
      (1, 2) -> True
      (_, _) -> False
```

Wonderfully concise code. However, it's hard to compile nested patterns like
that. In the compiler, we might simplify this to

``` haskell
    case x of
     (a, b) -> case a of
                 1 -> case b of
                        2 -> True
                        _ -> False
                 _ -> False
```

*note to future me, write a pattern matching compiler*

We've transformed our large nested pattern into a series of simpler, unnested
patterns. The benefit here is that this maps more straight to a series of
conditionals (or jumps).

Now one of the biggest decisions in any compiler is what to do with
expressions. We want to get rid of complicated nested expressions because
chances are our compilation target doesn't support them. In my
[second to last post][almost-last] we transformed a functional language into
something like SSA. In this post, we're going to walk through a different
intermediate representation: CPS.

## What is CPS

CPS is a restriction of how a functional language works. In CPS we don't have
nested expressions anymore. We instead have a series of lets which telescope out
and each binds a "flat" expressions. Additionally, no functions return. Instead,
they take a continuation and when they're about to return they instead pass
their value to it. This means that conceptually, all functions are transformed
from `a -> b` to `a -> (b -> void) -> void`. Logically, this is actually a
reasonable thing to do. This corresponds to mapping a proposition `b` to
`¬ ¬ b`. This has the effect that every "function call" can be compiled as a raw
jump, leading to a pretty clean implementation of tail call optimization.

This means we'd change something like

``` haskell
    fact n = if n == 0 then 1 else n * fact (n - 1)
```

into

``` haskell
    fact n k =
      if n == 0
      then k 1
      else let n' = n - 1 in
           fact n' (\r ->
                          let r' = n * r in
                          k r')
```

To see what's going on here we

 1. Added an extra argument to `fact`, it's return continuation
 2. In the first branch, we pass the result to the continuation instead of
    returning it
 3. In the next branch, we lift the nested expression `n - 1` into a flat let
    binding
 4. We add an extra argument to the recursive call, the continuation
 5. In this continuation, we apply multiply the result of the recursive call by
    `n` (Note here that we did close over `n`, this lambda is a real lambda)
 6. Finally, we pass the final result to the original continuation `k`.

The only tree-style-nesting here comes from the top `if` expression, everything
else is completely linear.

Let's formalize this process by converting Simply Typed Lambda Calculus (STLC)
to CPS form.

## STLC to CPS

First things first, we specify an AST for normal STLC

``` haskell
    data Tp = Arr Tp Tp | Int deriving Show

    data Op = Plus | Minus | Times | Divide
    data Exp a = App (Exp a) (Exp a) Tp
               | Lam Tp (Scope () Exp a)
               | Num Int
                 -- No need for binding here since we have Minus
               | IfZ (Exp a) (Exp a) (Exp a)
               | Binop Op (Exp a) (Exp a)
               | Var a
```

We've supplemented our lambda calculus with natural numbers and some binary
operations because it makes things a bit more fun. Additionally, we're using
bound to deal with bindings for lambdas. This means there's a terribly boring
monad instance lying around that I won't bore you with.


To convert to CPS, we first need to figure out how to convert our types. Since
CPS functions never return we want them to go to `Void`, the unoccupied
type. However, since our language doesn't allow `Void` outside of continuations,
and doesn't allow functions that don't go to `Void`, let's bundle them up into
one new type `Cont a` which is just a function from `a -> Void`. However, this
presents us with a problem, how do we turn an `Arr a b` into this style of
function? It seems like our function should take two arguments, `a` and `b ->
Void` so that it can produce a `Void` of its own. However, this requires
products since currying isn't possible with the restriction that all functions
return `Void`! Therefore, we supplement our CPS language with pairs and
projections for them.

Now we can write the AST for CPS types and a conversion between `Tp` and it.

``` haskell
    data CTp = Cont CTp | CInt | CPair CTp CTp

    cpsTp :: Tp -> CTp
    cpsTp (Arr l r) = Cont $ CPair (cpsTp l) (Cont (cpsTp r))
    cpsTp Int = CInt
```

The only interesting thing here is how we translate function types, but we
talked about that above. Now for expressions.

We want to define a new data type that encapsulates the restrictions of CPS. In
order to do this we factor out our data types into "flat expressions" and "CPS
expressions". Flat expressions are things like values and variables while CPS
expressions contain things like "Jump to this continuation" or "Branch on this
flat expression". Finally, there's let expressions to perform various operations
on expressions.

``` haskell
    data LetBinder a = OpBind Op (FlatExp a) (FlatExp a)
                     | ProjL a
                     | ProjR a
                     | Pair (FlatExp a) (FlatExp a)

    data FlatExp a = CNum Int | CVar a | CLam CTp a (CExp a)

    data CExp a = Let a (LetBinder a) (CExp a)
                | CIf (FlatExp a) (CExp a) (CExp a)
                | Jump (FlatExp a) (FlatExp a)
                | Halt (FlatExp a)
```

Let's let us bind the results of a few "primitive operations" across values and
variables to a fresh variable. Notice that here application is spelled `Jump`
hinting that it really is just a `jmp` and not dealing with the stack in any
way. This is a key idea with CPS: since function calls never return they need
not be actual function calls. They're all tail calls so we can treat them as
jumps and not overflow the stack as would be an issue with a normal calling
convention. To seal of the chain of function calls we have `Halt`, it takes a
`FlatExp` and returns it as the result of the program.

Expressions here are also parameterized over variables but we can't use bound
with them. Because of this we settle for just ensuring that each `a` is
globally unique.

## Logical Connections
## Wrap Up

[almost-last]: http://jozefg.bitbucket.org/posts/2015-03-24-pcf.html
