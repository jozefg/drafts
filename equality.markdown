---
title: Equality is Hard
---

Equality seems like one of the simplest things to talk about in a
theorem prover. After all, the notion of equality is something any
small child can intuitively grasp. The sad bit is, while it's quite
easy to hand-wave about, how equality is formalized seems to be a
rather complex topic.

In this post I'm going to attempt to cover a few of the main different
means of "equality proofs" or identity types and the surrounding
concepts. I'm opting for a slightly more informal approach in the
hopes of covering more ground.

## Definitional Equality

This is not really an equality type per say, but it's worth stating
explicitly what definitional equality is since I must refer to it
several times throughout this post.

Two terms `A` and `B` are definitional equal is a judgement notated

    Γ ⊢ A ≡ B

This is *not* a user level proof but rather a primitive, untyped judgement in
the meta-theory of the language itself. The typing rules of the
language will likely include a rule along the lines of

    Γ ⊢ A ≡ B, Γ ⊢ x : A
    ————————————————————–
         Γ ⊢ x : B

So this isn't an identity type you would prove something with, but a
much more magical notion that two things are completely the same to
the typechecker.

## Intensional

This is arguably the simplest form of equality. Identity types are
just normal inductive types with normal induction principles. The most
common is leibniz equality

    data Id (A : Type) : A → A → Type where
       Refl : (x : A) → Id x x

This yields a simple induction principle

    id-ind : (P : (x y : A) → Id x y → Type)
           → ((x : A) → P x x (Refl x))
           → (x y : A)(p : Id x y) → P x y p

In other words, if we can prove that `P` holds for the reflexivity
case, than `P` holds for any `x` and `y` where `Id x y`.

The fact that this only relies on simple inductive principles is also
a win for typechecking. Equality/substitution fall straight out of how
normal inductive types are handled! This also means that we can keep
decidability within reason.

The price we pay of course is that this is much more painful to work
with. An intensional identity type means the burden of constructing
our equality proofs falls on users. Furthermore, we lose the ability
to talk about observational equality.

Observational equality is the idea that two types are equal when they
can produce identical results no matter how they're used. It's clear
that we can prove that if `Id x y`, then `f x = f y`, but it's less
clear how to go the other way and prove something like

    fun_ext : (A B : Type)(f g : A → B)
             → ((x : A) → Id (f x) (g x)) → Id f g
    fun_ext f g p = ??

This can be introduced as an axiom but we have to sacrifice one of the
following

 1. Coherence
 2. Inductive types
 3. Extensionality
 4. Decidability




## Extensional


        Γ ⊢ Id(x, y)
        ———————————–
        Γ ⊢ x ≡ y

So if we can prove that two things are extensionally equal, then we
know that they are definitionally equal. This reflects our user level
identity type to the full powered definitional equality.

This provides a pleasant side effect that using equality types is now
quite straightforward. Once we have an identity type we can just
reflect it back up to definitional equality and start substituting
willy-nilly.

This extra power and implicit reflection also however, means that we
have to sacrifice decidable typechecking in most cases. extensional-LF
is completely undecidable for example.
