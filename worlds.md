---
title: Worlds in Twelf
tags: twelf, types
---

In this post I wanted to focus on one particular thing in Twelf:
`%worlds` declarations. They seems to be the most mysterious. I've had
a couple people tell me that they just blindly stick `%worlds () (x _
_ _)` before every total and pray which is a little concerning..

In this post hopefully we'll remove some of the "compile-n-pray" from
using Twelf code.

## What is %worlds

In Twelf we're interested in proving theorems. These theorems are
basically proven by some chunk of code that looks like this.

``` twelf
    my-cool-tyfam : with -> some -> cool -> args -> type.
    %mode my-cool-tyfam +A +B +C -D.

    some         : ... -> my-cool-tyfam A B C D.
    constructors : ... -> my-cool-tyfam A B C D.

    %worlds (...) (my-cool-tyfam _ _ _ _).
    %total (T) (my-cool-tyfam T _ _ _).
```

What's interesting here is the 3 directives we needed

 - `%mode` to specify which arguments of the type family are
   universally quantified and which are existentially qualified in our
   theorem. This specifies the "direction" of the type family, +
   arguments are inputs and - arguments are outputs.
 - `%total` which actually goes and proves the theorem by induction on
   the canonical forms of the term in the parens
 - `%worlds` which specifies the set of contexts to check the totality
   in. Note that a world is simply a set of contexts.

The one we're interested in talking about here is
`%worlds`. Everything we want to call `%total` has to have on of these
and as mentioned above it specifies the contexts to check the theorem
in. Twelf allows us to specify sets of contexts in a really powerful
way with "regular worlds".

Note that it actually matters which context we prove our theorem
in. LF doesn't have any support for inverting upon some term in the
context so we have to treat a variable as a canonical form in and of
itself. We can't just do `match` on it and argue that it's one of the
existing canonical forms. So for example remember how `plus` might be
defined.

``` twelf
    plus : nat -> nat -> nat -> type.
    %mode plus +N +M -P.

    plus/z : plus z N N.
    plus/s : plus N M P -> plus (s N) M (s P).
```

This is total in the empty context. If we added some `b : nat` to our
context then we have no way of showing it is either a `s` or a `z`!
This means that there's a missing case for variables of type `nat` in
our code. In order to exclude this impossible case we just assert that
we only care about `plus`'s totality in the empty context. This is
what the `%worlds` specification for `plus` stipulates

``` twelf
    %worlds () (plus _ _ _).
```

should be read as "`plus` should only be considered in the empty
context" so the only canonical forms of `plus` are those specified as
constants in our signature. This sort of specification is what we want
for most vanilla uses of Twelf.

For most cases we want to be proving theorems in the empty context
because we do nothing to extend the context in our
constructors. That's not to say that we *can't* specify some nonempty
world. We can specify a world where there is a `b : nat`, but if such
a `b` must appear we have a derivation `{a} plus b a z`. This way when
Twelf goes to check the canonical forms case for something in our
context, `b : nat`, it knows that there's a derivation that precisely
matches what we need. I'll circle back to this in a second, but first
we have to talk about how to specify fancier worlds.

## %block and Fancier Worlds

In Twelf there's some special syntax for specifying worlds. Basically
we can specify a template for some part of the world, called a
block. A world declaration is just a conglomeration of blocks and
Twelf will interpret this as a world of contexts in which each block
may appear zero or more times.

In Twelf code we specify a block with the following syntax

``` twelf
    %block block_name : block {a : ty} ... {b : ty'}.
```

This specifies that if there is an `a : ty` in the context, it's going
to be accompanied by a bunch of other stuff including a `b :
ty'`. Some blocks are pretty trivial. For example, if we wanted to
allow `plus` to be defined in a context with some `a : nat` in the
context we might say

``` twelf
    %block random_nat : block {b : nat}.
    %worlds (random_nat) (plus _ _ _).
```

This doesn't work though. If we ask Twelf to check totality it'll get
angry and say

    Coverage error --- missing cases:
    {#random_nat:{b:nat}} {X1:nat} {X2:nat} |- plus #random_nat_b X1 X2.

In human,

> You awful person Danny! You're missing the case where you have to
> random integers and the random natural number `b` from the
> random_nat block and we want to compute `plus b X X'`.

Now there are a few things to do here. The saner person would probably
just say "Oh, I clearly don't want to try to prove this theorem in a
nonempty context". *Or* we can wildly add things to our context in
order to patch this hole. In this case, we need some proof that about
adding `b` to other stuff. Let's supplement our block

```
    %block random_nat : block {b : nat}{_:{a} plus b a z}
```

Such a context is pretty idiotic though since there isn't a natural
number that can satisfy it.

## Conclusion
