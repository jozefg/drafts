---
title: What Are Impredicative Types?
tags: haskell, types
---

So the results from Stephen's [poll][poll] are in! Surprisingly,
impredicative types topped out the list of type system extensions
people want to talk about so I figured I can get the ball rolling.

First things first, all the Haskell code will need the magical
incantation

``` haskell
    {-# LANGUAGE ImpredicativeTypes #-}
```

## What Is Impredicative Polymorphism

We have a lot of extensions that make polymorphism more flexible in
Haskell, `RankNTypes` and `Rank2Types` spring to mind. However, one
important feature lacking is "first class polymorphism".

With impredicative polymorphism `forall`'s become a normal type like
any other. We can embed them in structures, toss them into polymorphic
functions, and generally treat them like any other type.

Readers with a mathematical background will wonder why these are
called "impredicative" types then. The idea is that since we can have
polymorphic types embedded in other structures, we could have
something like

``` haskell
    type T = (Int, forall a. a -> Int)
```

That `a` could assume any time *including `T`*. So each type
definition can quantify over itself which nicely corresponds to the
mathematical notion of impredicativity.

One simple example where this might come up is when dealing with
lenses. Remember lenses have the type

``` haskell
    type Lens {- viciously -} s t a b = forall f. (a -> f b) -> s -> f t
```

If we were to embed lenses in let's say a tuple,

``` haskell
    type TLens a b = (Lens a a (a, b) (a, b), Lens b b (a, b) (a, b))

    foo :: TLens Int Bool
    foo = (_1, _2)
```

We'd need impredicative types because suddenly a polymorphic type has
appeared within a structure.

## Why No One Uses It

Now that we've seen how amazing impredicative polymorphism, let's talk
about how no one uses it. There are two

## Wrap Up


[poll]: http://www.stephendiehl.com/posts/poll.html
