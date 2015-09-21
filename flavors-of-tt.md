---
title: Two Different Flavors of Type Theory
tags: types
---

So summer seems to be about over. I'm very happy with mine, I learned
quite a lot. In particular over the last few months I've been reading
and fiddling with a different kind of type theory than I was used to:
computational type theory. This is the type theory that underlies
Nuprl (or [JonPRL][jonprl] cough cough).

One thing that stood out to me was that you could do all these
absolutely *magical* things in this system that completely flew in the
face of what I was used to after 3 or so years of Coq and Agda. In
this post I'd like to sketch some of the philosophical differences
between CTT and a type theory more in the spirit of CiC. I'd like to
introduce some folks to the other side of the aisle and write down the
solutions to some misconceptions I've had for a while.

## Formal Type Theory and Props-as-Types #1

First things first, let's go over the more familiar notion of type
theory. To develop one of these type theories you start by discussing
some syntax. You lay out the syntax for some types and some terms

    A ::= Σ x : A. A | Π x : A. A | ⊤ | ⊥ | ...
    M ::= M M | λ x : A. M | <M, M> | π₁ M | ⋆ | ...

And now we want to describe the all important `M : A` relation. This
tells us that some term has some type. This relation is inductively
defined from a finite set of inferences. Ideally, it's even decidable
for philosophical reasons I've never cared too much about.

In fact, it's this relation that really governs our whole type theory,
everything else is going to stem from this. As an afterthought, we may
decide that we want to identify certain terms which other terms this
is called definitional equality. It's another inductively defined (and
decidable) judgment `M ≡ N : A`. Two quick things to note here

 1. Definitional equality is completely arbitrary; it exists in the
    way it does because we defined it that way and for no other reason
 2. The complexity of proving `M ≡ N : A` is independent of the
    complexity of `A`

The last point is some concern because it means that equality for
functions is never going to be right for what we want. We have this
uniformly complex judgment `M ≡ N : A` but when `A = Π x : B. C` the
complexity *should* be greater and dependent on the complexity of `B`
and `C`. That's how it works in math after all, equality at functions
is defined pointwise, something we can't really do here if `≡` is to
be decidable or just be of the same complexity no matter the type.

Now we can do lots of things with our theory. One thing we almost
always want to do is now go back and build an operational semantics
for our terms. This operational semantics should be some judgment
`M ↦ M` with the property that `M ↦ N` will imply that `M ≡ N`. This gives
us some computational flavor in our type theory and lets us run the
pieces of syntax we carved out with `M : A`.

But these terms that we've written down aren't really
programs. They're just serializations of the collections of rules
we've applied to prove a proposition. There's no ingrained notion of
"running" an `M` since it's built on after the fact. What we have
instead is this `≡` relation which just specifies which symbols we
consider equivalent but even it is pretty arbitrary. There's no reason
we `≡` needs to be a reasonable term rewriting system or anything. If
we're good at our jobs it will be, sometimes (HoTT) it's not
completely clear what that computation system is even though we're
working to find it.

This leads to the first interpretation of the props-as-types
correspondence (the boring one). This states that the inductively
defined judgments of a logic give rise to a type theory whose terms
are proof terms for those same inductively defined judgments. It's an
identification of similar looking syntactic systems. It's useful to be
sure if you want to develop a formal type theory, but it gives us less
insight into the computational nature of a logic because we've
reflected into a type theory which we have no reason to suspect has a
reasonable computational characterization.

*It was pointed out to me that there is a very large gap between
 decidable and efficiently computable. Many, many things in formal
 type theory are inefficient to compute*.

## Behavioural/Computational Type Theory and Props-as-Types #2

Now we can look at a second flavor of type theory. In this setting the
way we order our system is very different. We start with an
programming language, a collection of terms and an untyped evaluation
relation between them. We don't necessarily care about all of what's
in the language. As we define types later we'll say things like "Well,
the system has to include at least X" but we don't need to
exhaustively specify all of the system. It follows that we have
actually no clue when defining the type theory how things
compute. They just compute *somehow*. We don't really even want the
system to be strongly normalizing, it's perfectly valid to take the
lambda calculus (or Perl... actually can someone please make this a
thing?).

So we have some terms and `↦`, on top of this we start by defining a
notion of equality between terms. This equality is purely
computational and has no notion of types yet (like `M ≡ N : A`)
because we have no types yet. This equality is sometimes denoted `~`,
we usually define it as `M ~ N` iff `M ↦ O(Ms)` if and only if `N ↦
O(Ns)` and if they terminate than `Ms ~ Ns`. By this I mean that two
terms are the same if they compute in the same way, either by
diverging or running to the same value built from `~` equal
components. For more on this, you could read [Howe's paper][howe].

So now we still have a type theory with no types.. It's about time to
add those. The next distinguishing feature of computational type
theory is that we're not going to list out a bunch of inferences and
have that be our type theory. Rather, for each new type we answer
several questions about it,

 1. What other values denote types equal to it?
 2. What values are in the type?
 3. What values are considered equal **at that type**?

The first questions is usually answered in a boring way, for instance,
we would say that `Π x : A. B = Π x : A'. B'` if we know that `A = A'`
and `B = B'` under the assumption that we have some `x ∈ A`. We then
specify two and three. There we just give the rules for demonstrating
that some value, which is a program existing entirely independently of
the type we're building, is in the type. Continuing with functions, we
might state that

       e ∈ B (x ∈ A)
    ———————————————————
    λx. e ∈ Π x : A. B

Here I'm using `_ (_)` as syntax for a hypothetical judgment, we have
to know that `e ∈ B` under the assumption that we know that `x ∈
A`. Next we have to decide what it means for two values to be equal as
functions. We're going to do this behaviourally, by specifying that
they behave as equal programs when used as functions. Since we use
functions by applying them all we have to do is specify that they
behave equally on application


     v x = v' x ∈ B (x ∈ A)
    ————————————————————————
      v = v' ∈ Π x : A. B

Equality is determined on a per type basis by induction over the
type. This means that when defining equality for `Π x : A. B` we get
to use the equalities for `A` and `B`! We proceed in this fashion,
defining rules on a per-type basis with induction(-recursion). As
another example, let's have a look at the equality type.


     A = A'  a = a' ∈ A  b = b' ∈ A
     ————————————————————————————————
        I(a; b; A) = I(a'; b'; A')


       a = b ∈ A
     ———————————————
      ⋆ ∈ I(a; b; A)

        a = b ∈ A
     ———————————————————
      ⋆ = ⋆ ∈ I(a; b; A)

Things to notice here, first off the various rules depend on the rules
governing membership and equality in `A` as we should
expect. Secondly, `⋆` (the canonical occupant of `I(...)`) has no type
information. There's no way to reconstruct whatever reasoning went
into proving `a = b ∈ A` because there's no computational content in
it. The thing on the left of the `∈` only describes the portions of
our proof that involve computation and equalities in computational
type theory are always computationally trivial. Therefore, they get
the same witness no matter the proof, no matter the types
involved. Finally, the infamous equality reflection rule is really
just the principle of inversion that we're allowed to use in reasoning
about hypothetical judgments.

This leads us to the second cast of props-as-types. This one states
that constructive proof has computational character. Every proof that
we write in a logic like this gives us back an (untyped) program which
we can run as appropriate for the theorem we've proven. This is the
idea behind Kleene's realizability model. Similar to what we'd do with
a logical relation we define what each type means by defining the
class of appropriate programs that fit its specification. For example,
we defined functions to be the class of things that apply and
proofs of equality are ⋆ when the equality is true and there are no
proofs when it's false.

## Building Proof Assistants for Formal Type Theory

## Building Proof Assistants for Computational Type Theory

## Wrap Up

[howe]: http://www.nuprl.org/documents/Howe/EqualityinLazy.html
