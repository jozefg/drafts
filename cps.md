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

Let's formalize this process by converting System F to CPS form.

## What's System F Again?

System F is the smallest functional language you can imagine with
polymorphism. It's the core calculus behind a language like ML.

Real System F has only 5 constructions, variables, abstraction, application,
type abstraction, and type application. The distinguishing feature here is the
type abstraction/application. In Haskell or SML we just have polymorphism, it's
not a thing you see at the expression level but if you write your expressions in
a certain way hey presto, they're polymorphic. Likewise, you just *use* a
polymorphic expression as if its the type you want and it works itself out.

In System F, this process is made explicit. To write something polymorphic we
start with it with `Λ τ.` this gives us a fresh type `τ` which we use throughout
the expression. Let's say we have `e : A` where `τ` appears somewhere in
`A`. Then `Λ τ. e`, then the whole thing has the type `Λ τ. e : ∀ τ. A`.

## System F to CPS


[almost-last]: http://jozefg.bitbucket.org/posts/2015-03-24-pcf.html
