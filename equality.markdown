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

Two terms `A` and `B` are definitional equal is a judgment notated

    Γ ⊢ A ≡ B

This is *not* a user level proof but rather a primitive, untyped judgment in
the meta-theory of the language itself. The typing rules of the
language will likely include a rule along the lines of

    Γ ⊢ A ≡ B, Γ ⊢ x : A
    ————————————————————–
         Γ ⊢ x : B

So this isn't an identity type you would prove something with, but a
much more magical notion that two things are completely the same to
the typechecker.

Now in most type theories we have a slightly overgrown notion of
definitional equality where not only are `x ≡ y` if `x` is `y` only by
definition but also by computation.

So in Coq for example

    (2 + 2) ≡ 4

Even though definitionally these are entirely separate entities. In
type theories that distinguish between the two, the judgment that
when normalized `x` is `y` is called judgemental equality. I won't
distinguish between the two further because most don't, but it's worth
noting that they're technically separate concepts.

## Propositional Equality

This is the sort of equality that we'll spend the rest of our time
discussing. Propositional equality is a particular proposition with
the type

    Id : (A : Set) → A → A → Type

We should be able to prove a number of definitions like

    reflexivity  : (A : Set)(x     : A) → Id x x
    symmetry     : (A : Set)(x y   : A) → Id x y → Id y x
    transitivity : (A : Set)(x y z : A) → Id x y → Id y z → Id x z

This is an entirely separate issue from definitional equality since
propositional equality is a userland concept.

One very important difference is that we can make proofs like

    sanity : Id 1 2 → ⊥

Since the identity proposition is a type family which may or may not
be inhabited.

## Intensional

This is arguably the simplest form of equality. Identity types are
just normal inductive types with normal induction principles. The most
common is leibniz equality

    data Id (A : Set) : A → A → Type where
       Refl : (x : A) → Id x x

This yields a simple induction principle

    id-ind : (P : (x y : A) → Id x y → Type)
           → ((x : A) → P x x (Refl x))
           → (x y : A)(p : Id x y) → P x y p

In other words, if we can prove that `P` holds for the reflexivity
case, than `P` holds for any `x` and `y` where `Id x y`. Tangent:
often times `id-ind` is referred to as `J`, I have no clue why but
there you are.

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

    fun_ext : (A B : Set)(f g : A → B)
             → ((x : A) → Id (f x) (g x)) → Id f g
    fun_ext f g p = ??

This can be introduced as an axiom but we have to sacrifice one of the
following

 1. Coherence
 2. Inductive types
 3. Extensionality
 4. Decidability

This strikes at the core issue of why intensional type theory doesn't
always work well: it's concerned with how things are constructed vs
how they're used.

## Definitional + Extensional

Some type theories go the opposite route in the hope to regain that
missing extensionality. One of those type theories is extensional type
theory.

In the simplest formulation, we have intensional type theory with a
new rule, reflection

    Γ ⊢ Id x y
    ——————————–
    Γ ⊢ x ≡ y

This means that our normal propositional equality can be shoved *back*
into the more magical definitional equality. This gives us a lot more
power, all the typecheckers magic and support of definitional equality
can be used with our equality types!

There are two main drawbacks however

 1. Magical Identity Types
 2. Potentially Undecidable

With this flavor of extensional type theory our type of equality must
be known to the typechecker so it can perform reflection, this means
that a user can't define his or her own equality as in standard
intensional type theory.

Furthermore, arbitrary reflection can also make things undecidable in
general. For example Martin Lof's system is undecidable in with
extensional equality.

## Propositional Extensionality

This is another flavor of extensional type theory which is really just
intensional type theory plus some axioms.

We can arrive at this type theory in a number of ways, the simplest is
to add axiom K

    k : (A : Set)(x : A)(P : (x : A) → Id x x → Type)
      → P x (Refl x) → (p : Id x x) → P x p

This says that if we can prove that for any property `P`,
`P x (Refl x)` holds, then it holds for any proof that `Id x x`. This
is subtly different than straightforward induction on `Id` because
here we're not proving that a property parametrized over two
different values of `A`, but only one.

This is horribly inconsistent in something like homotopy type theory
but lends a bit of convenience to theories where we don't give `Id` as
much meaning.

Using `k` we can prove that for any `p q : Id x y`, then `Id p q`.
In Agda notation

``` agda
    prop : (A : Set)(x y : A)(p q : x ≡ y)
         → p ≡ q
    prop A x .x refl q = k A P (λ _ → refl) x q
      where P : (x : A) → x ≡ x → Set
            P _ p = refl ≡ p
```

This can be further refined to show that that we can eliminate all
proofs that `Id x x` are `Refl x`

``` agda
    rec : (A : Set)(P : A → Set)(x y : A)(p : P x) → x ≡ y → P y
    rec A P x .x p refl = p

    rec-refl-is-useless : (A : Set)(P : A → Set)(x : A)
                        → (p : P x)(eq : x ≡ x) → p ≡ rec A P x x p eq 
    rec-refl-is-useless A P x p eq with prop A x x eq refl
    rec-refl-is-useless A P x p .refl | refl = refl
```

This form of extensional type theory still leaves a clear distinction
between propositional equality and definitional equality by avoiding a
reflection rule. However, with `rec-refl-is–useless` we can do much of
the same things, whenever we have something that matches on an
equality proof we can just remove it.

## Heterogeneous Equality

The next form of equality we'll talk about is slightly different than
previous ones. Heterogeneous equality is designed to co-exist in some
other type theory and supplement the existing form of equality.

Heterogeneous equality is most commonly defined with John Major
equality

``` agda
    data JMeq : (A B : Set) → A → B → Set where
      JMrefl : (A : Set)(x : A) → JMeq A A x x
```

This is termed after a British politician since while it promises that
any two terms can be equal regardless of their class (type), only two
things from the same class can ever be equal.

`JMeq` is usually paired with an axiom to reflect heterogeneous
equality back into our normal equality proof.

    reflect : (A : Set)(x y : A) → JMeq x y → Id x y

This reflection doesn't look necessary, but arises for similar reasons
that dictate that `k` is unprovable.

It looks like this heterogeneous equality is a lot more trouble than
it's worth at first. It really shines when we're working with terms
that we *know* must be the same, but require pattern matching or other
jiggering to prove.

## Wrap Up

So this has been a whirlwind tour through a lot of different type
theories. I partially wrote this to gather some of this information in
one (free) place. If there's something here missing that you'd like to
see added, feel free to comment or email me.
