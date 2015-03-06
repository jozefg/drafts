---
title: Haskell Doesn't Have Sums
tags: haskell
---

So here's a write up of some folklore I've heard at CMU. A couple
people have told me separately that "Haskell doesn't have sums". This
confused me for a good long while because to me sums were just

``` haskell
    data Sum a b = Inl a | Inr b
```

It didn't seem like there was a whole lot of a wiggle room! It turns
out to properly explain why this is the case we need to formalize our
handwavy notion of sums.

## What Are Sums

Let's turn to category theory for some inspiration here. In category
theory we capture the notion of sums with the following diagram


    A ————→ A + B ←———— B
     \        |        /
      \       |       /
       \      |      /
        \     |     /
         \    |    /
          \   ↓   /
              C


So a sum of two objects is something that makes this diagram
commute. In haskell code we know that `A + B` is `Either A B`. the top
two arrows are just `Left` and `Right`. This means we need to find
some arrow `Either A B -> C` if we have an arrow from `A -> C` and an
arrow from `B -> C`. Sound familiar? That's just `either`

``` haskell
    either :: (a -> c) -> (b -> c) -> Either a b -> c
```

More than this, we want to be sums unique up to isomorphism. This
means that we want `A + B` to some how by the canonical object for
this diagram. To express this we say that there is some `D` that could
be used in place of `A + B` here then there is a unique
`h : A + B → D`
that makes this diagram commute. That is, if we have a type `D` and
some functions `f :: A -> D`, `g :: B -> D`, and `h :: D -> C`, then

## Domain Theory

## Conclusion
