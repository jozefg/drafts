---
title: Some Fun with Agda
tags: agda, types
---

To celebrate the end of (most) of my finals this weekend I decided to write some
new and fun Agda code. Specifically I wanted to show how you'd write what's
called a decision procedure in Agda.

A decision procedure is not an altogether exciting thing. In most language it's
just a function that returns `true` or `false` for a given problem. These come
up a lot in computability theory or whatever. In fact, most people have heard of
at least the halting problem which boils down to the impossibility of writing a
particular decision procedure.

In a dependently typed language though, we can make them a bit more
fun. Specifically, instead of returning true or false what if we returned
something a bit more convincing.

Since we have the ability to write very expressive propositions in our types, we
can swap out the boring notion of booleans for

``` agda
    data Dec (P : Set) : Set where
      yes : P   → Dec P
      no  : ¬ P → Dec P
```

So if I give you an expression of type `Dec P`, it's either a proof that `P`
is inhabited (by actually having a term of type `P`) or a refutation that `P`
cannot be inhabited by showing `P` implies false (`⊥`). Let's say we want to
write a function which tells us whether there's an element in a list. Rather
than just having it return `true`/`false`, we can have it return something like

``` agda
    -- I suppose A needs decidable equality but whatever
    find : {A : Set}(ls : List A)(a : A) → Dec (ElementInList ls a)
```

The advantage here is two-fold

 1. Different questions have semantically different answers and now our types
    reflect that
 2. The correctness of `find` is immediate from the fact that it's well typed

We know that `find` must be correct because it's somehow producing or refuting
evidence that the element is in the list! If we really just want a yes or no
answer we don't need to use the evidence it gives us but having it makes
correctness easy to believe.

In this post I'll illustrate writing a fancily-typed (can this be a real
phrase?) decision procedure for the subset sum problem. To be honest, I wanted
this to be the fun making change problem that comes up in the first chapter of
SICP so the names reflect that.

## Defining The Problem

We first start by defining a few of the key types we're going to need for our
problem.

``` agda
    Coin : Set
    Coin = ℕ

    Purse : Set
    Purse = List Coin
```

## Writing The Decision Procedure
