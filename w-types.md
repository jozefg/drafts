---
title: W[eird] Types
tags: types, agda
---

I haven't posted any of the fun Agda code I've been writing lately,
now seems like as good a time as any to change it!

Let's go over a fun little gem: encoding all well founded types as
nonrecursive types fed to one monstrous type: `W`. I'm reasonably
certain this trick dates back to Martin-Lof, but don't quote me.

First let's start by looking at the classic recursive type: `Nat`.
Each `Nat` is either the successor of another or zero. We can
therefore visualize a `Nat` as

    S - S - S - S - Z

A tree of constructor applications which terminates as `Z`. Really, we
can imagine formatting every sane recursive type as a tree. Each
node is a tuple of the non-recursive arguments given to a constructor
and a node's children are the recursive bits of that node's
constructor. For `Nat` there are only recursive arguments so it looks
like a list of `S`'s followed be a `Z`. For an actual list, we'd have

    Cons 1 - Cons 2 - Cons 3 - Nil

And for actual trees we might have

         Node 'a'
            |
          -- --
         |     |
     Node 'b' Leaf
         |
       -- --
      |     |
     Leaf  Leaf

So we have this pattern that seems to capture a lot of useful
inductive types. Can we write this down as it's own inductive
definition?

Well we have `n` types of nodes, annotated with some stuff and
depending on what type of node different numbers of children. If we
represent the types of nodes with

``` agda
    data W (A : Set) (F : A → Set) : Set where
      sup : A → (F a → W A F) → W A F
```

Each node has the type `a : A` and `F` determines the shape of the
recursive calls given the type of the node.

For example, for natural numbers we have two types of nodes which we
represent with `bool`. For the successor case we have only one
recursive call. What sort of argument should a function take if it can
only ever return 1 answer? `⊤` of coures! When we're in the `Z` case
there are no recursive calls so `F false` must be uninhabitable. This
means

``` agda
    NatF : Bool → Set
    NatF true = ⊤
    NatF false = ⊥
```

We can then define

``` agda
    Nat : Set
    Nat = W Bool NatF
```
