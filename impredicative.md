---
title: What Are Impredicative Types
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

By that I mean that a fully polymorphic type won't unify with a type
variable. Instead, Haskell will demote it to a fresh type variable and
later it may be generalized. To see this in action, notice how

``` haskell

```

## Why No One Uses It

## Wrap Up


[poll]: http://www.stephendiehl.com/posts/poll.html
