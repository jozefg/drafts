---
title: Notes on Quotients Types
tags: types
---

Lately I've been reading a lot of type theory literature. In effort
to help my future self, I'm going to jot down a few thoughts on
quotient types, the subject of some recent google-fu.

## But Why!

The problem quotient types are aimed at solving is actually a very
common one. I'm sure at some point or another you've used a piece of
data you've wanted to compare for equality. Additionally, that data
properly needed some work to determine whether it was equal to
another piece.

A simple example might would be representing rational numbers. A
rational number is a fraction of two integers, so let's just say

``` haskell
    type Rational = (Integer, Integer)
```

Now all is well, we can define a `Num` instance and what not. But what
about equality? Clearly we want equivalent fractions to be equal. That
should mean that `(2, 4) = (1, 2)` since they both represent the same
number.

Now our implementation has a sticky point, clearly this isn't the case
on its own! What we really want to say is "`(2, 4) = (1, 2)` up to
normalization".

Haskell's own `Rational` type solves this by not exposing a raw
tuple. It still essentially exists under the hood, but we only expose
smart constructors that will reduce our fractions as far as possible.

This is displeasing from a dependently typed setting however, we want
to be able to formally prove the equality of some things. This
"equality modulo normalization" leaves us with a choice. Either we can
really provide a function which is essentially

``` agda
    foo : (a b : Rational)
        -> Either (reduce a = reduce b) (reduce a /= reduce b)
```

This doesn't really help us though, there's no way to express that `a`
should be observationally equivalent to `b`. This is a problem
seemingly as old as dependent types: How can we have a simple
representation of equality that captures all the structure we want and
none that we don't.

Hiding away the representation of rationals certainly buys us
something, we can use a smart constructor to ensure things are
normalized. From there we could potentially prove a (difficult)
theorem which essentially states that

``` agda
    =-with-norm : (a b c d : Integer)
                -> a * d = b * c -> mkRat a b = mkRat c d
```

This still leaves us with some woes however, now a lot of computations
become difficult to talk about since we've lost the helpful notion
that `denominator o mkRat a = id` and similar. The lack of
transparency shifts a lot of the burden of proof onto the code privy
to the internal representation of the type, the only place where we
know enough to prove such things.

Really what we want to say is "Hey, just forget about a bit of the
structure of this type and just consider things to be identical up to
`R`". Where `R` is some equivalence relation, eg

  1. `a R a`
  2. `a R b` implies `b R a`
  3. `a R b` and `b R c` implies `a R c`

If you're a mathematician, this should sound similar. It's a lot like
how we can take a set and partition it into equivalence classes. This
operation is sometimes called "quotienting a set".

For our example above, we really mean that our rational is a type
quotiented by the relation `(a, b) R (c, d)` iff `a * c = b * d`.

Some other things that could potentially use quotienting

 - Sets
 - Maps
 - Integers
 - Lots of Abstract Types

Basically anything where we want to hide some of the implementation
details that are irrelevant for their behavior.

## More than Handwaving

Now that I've spent some time essentially waving my hand about
quotient types what are they? Clearly we need a rule that goes
something like

     Γ ⊢ A type, E is an equivalence relation on A
    ———————————————–———————————————————————————————
            Γ ⊢ A // E type

Along with the typing rule

        Γ ⊢ a : A
    ——————————————————
      Γ ⊢ a : A // E

So all members of the original type belong to the quotiented type, and
finally

      Γ ⊢ a : A, Γ ⊢ b : A, Γ ⊢ a E b
    –——————————————–——————————————————
             Γ ⊢ a ≡ b : A // E

Notice something important here, that `≡` is the fancy shmancy
definitional equality baked right into the language. This calls into
question decidability. It seems that `a E b` could involve some
non-trivial proof terms.

More than that, in a constructive, proof relevant setting things can be
a bit trickier than they seem. We can't just define a quotient to be
the same type with a different equivalence relation, since that would
imply some icky things.

Consider some predicate `P` so that `a E b` means
`P(a) → P(b) ∧ P(b) → P(a)`. Now while both `P(a)` and `P(b)`
are either occupied or unoccupied, we certainly can't conclude they're
equal. For example is `P` is a map to `̱Nat`, then naively assuming
their equal could easily lead us to a situation when `1 ≡ 2`.

However, if we define the quotiented type as a simple redefinition of
equivalence, then we have `a E b` means `a ≡ b` means `P(a) ≡ P(b)` by
a simple application of `cong`. Oops.

Clearly some subtler treatment of this is needed. To that end I found
[this paper][that-paper] discussing some of the handling of NuRPL's
quotients enlightening.

## How NuPRL Does It

The paper I linked to is a discussion on how to think about quotients
in terms of other type theory constructs. In order to do this we need
a few things first.

The first thing to realize is that NuPRL's type theory is different
than what you are probably used to. We don't have this single magical
global equality. Instead, we define equality inductively across the
type. This notion means that our equality judgment doesn't have to be
natural in the type it works across. It can do specific things at
corner cases. Perhaps the most frequent is that we can have functional
extensionality.

    f = g ↔ ∀ a. f a = g a

Okay, so now that we've tossed aside the notion of a single global
equality, what else is new? Well something new is the lens through
which many people look at NuRPLs type theory: PER semantics. Remember
that PER is a relationship satisfying

  1. `a R b → then b R a`
  2. `a R b ∧ b R c → a R c`

In other words, a PER is an equivalence relationship that isn't
necessarily reflexive at all points.

The idea is to view types not as some opaque "thingy" but instead to
be partial equivalence relations across the set of untyped lambda
calculus terms. Inductively defined equality falls right out of this
idea since we can just define `a ≡ b : A` to be equivalent to
`(a, b) ∈ A`.

Now another problem rears it head, what does `a : A` mean? Well even
though we're dealing with PERs it's important to maintain reflexivity
across each type, we still want `1 ≡ 1`! So we can therefore define
`a : A` to be `(a, a) ∈ A`.

Another important constraint, in order for a type family to be well
formed, it needs to respect the equality of the type it maps
across. In other words, for all `B : A → Type`, we have
`(a, a') ∈ A' ⇒ B a ≡ B a'`. This should seem on par with how we
defined function equality.

Now with all of that out of the way, I'd like to present two typing
rules

      Γ ⊢ A ≡ A';  Γ, x : A, y : A ⊢ E[x; y] = E'[x; y]; E and E' are PERS
      ————————————————————————————————————————————————————————————————————
                          Γ ⊢ A ‌// E ≡ A' // E'

In English, two quotients are equal when the types and there quotients
are equal.

     Γ, u : x ≡ y ∈ (A // E), v :  ∥ x E y ∥, Δ[u] ⊢ C [u]
     ———————————————————————————————————————————————————–
           Γ, u : x ≡ y ∈ (A // E), Δ[u] ⊢ C [u]

There are a few new things here. The first is that we have a new
`Δ [u]` thing. This is a result of dependent types, can have things in
our context that depend on `u` and so to indicate that we "split" the
context, with `Γ, u, Δ` and apply the depend part of the context `Δ`
to the variable it depends on `u`.

Now the next new thing is that `∥ A ∥` is an "extensional squash
operator". The idea is that it should be occupied if and only if `A`
has any inhabitants. The analogy I've sometimes seen is to double
negation, ie `(A → ⊥) → ⊥`. However there's something special about
our squash operator, we can "unsquash" it when trying to prove
equalities.

This is because in Martin Lof type theory (an consequently NuPRL)
equality proofs only every yield trivial terms. We're not interested
in how they're constructed just that they are constructed. This means
that since we're going to throw away the proof term anyways we can
unbox the other proof terms we've thrown away.

Now the long and short of this is that when we're of this is that when
we're trying to use an equivalence between two terms in a quotient, we
only get the squashed term. This done mean that we only need to
provide a squash to get equality in the first place though

    Γ ⊢ ∥ x E y  ∥; Γ ⊢ x : A; Γ ⊢ y : A
    ——————————————————————————————————–
          Γ ⊢ x ≡ y : A // E

Remember that we can trivially form an `∥ A ∥` from `A`'.

Now there's just one thing left to talk about, using our quotiented
types. To do this the paper outlines one primitive elimination rule
and defines several others.

    Γ, x : A, y : A, e : x E y, a : ND, Δ[ndₐ{x;y}] ⊢ |C[ndₐ{x;y}]|
    ——————————————————————————————————————————————————————————————–
                   Γ, x : A // E, Δ[x] ⊢ |C[u]|

So this looks a bit weird, let's go over this one part at a time

 - `ND`

     `ND` is a admittedly odd type that's supposed to represent
     nondeterministic choice. It has two terms, `tt` and `ff` and
     they're considered "equal" under `ND`. However, `nd` returns its
     first argument if it's fed `tt` and the second if it is fed
     `ff`. Hence, nondeterminism.

     Now in our rule we use this to indicate that if we're eliminating
     some quotiented type we can get *any* value that's considered
     equal under `E`.

 - `|C[u]|`

    This is the intensional version of the squash operator. It has two
    basic rules,

          Γ ⊢ A
         ———————
         Γ ⊢ [A]

           Γ, x : |A|, Δ[̱•] ⊢ C[̱•]
         ——————————————————————————
           Γ, x : |A|, Δ[x] ⊢ C[x]

    Where • is the trivial occupant.

So this says that if we can prove that `C` is occupied for some `y`
related to `x` by `E`, then it's occupied for `C`.

## Wrap up

As with my last post, here's some questions for the curious reader to
pursue

 - What elimination rules can we derive from the above?
 - If we're of proving equality can we get more expressive rules?
 - What would an extensional quotient type look like?
 - Why would we want intensional or extensional?
 - How can we express quotient types with higher inductive types from
   HoTT

The last one in particularly interesting.

[that-paper]: http://www.nuprl.org/documents/Nogin/QuotientTypes_02.pdf
