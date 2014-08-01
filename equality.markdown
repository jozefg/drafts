---
title: Equality is Hard
---

Equality seems like one of the simplest things to talk about in a
theorem prover. After all, the notion of equality is something any
small child can intuitively grasp. The sad bit is, while it's quite
easy to hand-wave about, how equality is formalized seems to be a
rather wide field at the moment.

In this post I'm going to attempt to cover a few of the main different
means of "equality proofs" or identity type.

## Definitional Equality

This is not really an equailty type per say, but it's worth stating
explicitly what definitional equality is since I must refer to it
several times throughout this post.

Two terms `A` and `B` are definitional equal is a judgement notated

    Γ ⊢ A ≡ B

This is *not* a user level proof but rather a primitive judgement in
the meta-theory of the language itself. The typing rules of the
language will likely include a rule along the lines of

    Γ ⊢ A ≡ B, Γ ⊢ x : A
    ————————————————————–
         Γ ⊢ x : B

So this isn't an identity type you would prove something with, but a
much more magical notion that two things are completely the same to
the typechecker.

It arises from a few principles, like inlining definitions, rewriting
other definitionally equal terms, and the like.

## Extensional

The first major cross-roads we come to is whether identity types
should be extensional or intensional. If they are extensional, then
we can view them as something like

    Id   : (A : Type) → A × A
    Id A = { ... list of all things are equal in A ... }

Furthermore, the notation that that `Id(x, y)` is really just
something like `(x, y) ∈ Id A`. Extensional equality also comes with a
metajudgement that holds special weight

        Γ ⊢ Id(x, y)
        ———————————–
        Γ ⊢ x ≡ y

So if we can prove that two things are extensionally equal, then we
know that they are definitionally equal. This reflects our user level
identity type to the full powered definitional equality.

