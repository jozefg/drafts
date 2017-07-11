---
title: Intuitionism, Fancy Types and Computability
tags: types
---

I haven't been writing very often (meaning for the last 9
months). This is in part due to some health issues and the general
chaos that is an undergrad degree at CMU. However, it's also been
because I've been dumping a lot of my free time into reading rather
than writing. Every once in a while I have the though "How do I not
know X yet" and so then I end up going and reading a book on X. With
the cost of textbooks incidentally this is not a cheap hobby :/

One topic in particular that I've been curious about is the
relationship between computability, continuity, and type theory. In
particular, as type theorists we tend to adopt a constructive
view-point. There are two ways to think of this:

 1. Computation is limited, many things are uncomputable. If we want
    our proofs to come with some sort of computational character there
    are many things we many not prove. In particular, the law of the
    excluded middle is out of reach.
 2. Computation allows us to assume far too many things about "what
    computation may do" to allow everything. In particular, at higher
    types, we assume that only a finite number of questions can be
    asked by a computation. In order to make this assumption valid, we
    must ensure that this is actually what takes place at lower
    types.

The first perspective is doubtless more common, but it's kind of a sad
line to take. After all, it's a poor selling point for a mathematician
who doesn't care about computation. We seem to have just imposed an
arbitrary limit on what they can prove in order to maintain
consistency with a model that they don't care about! So perhaps the
second perspective is more interesting. To clarify what I mean by at
though I should make some definitions a bit more explicit. When I talk
about "higher type" I mean functions which are increasingly higher
order. This turns out to be easiest to do by example,

``` haskell
    order1 : ℕ → ℕ
    order2 : (ℕ → ℕ) → ℕ
    order3 : ((ℕ → ℕ) → ℕ) → ℕ
    ...
```

The point that I want to make is that you can compute more at order
`n + 1` if you can compute less at order `n`. In particular, our harsh
requirements of computability are going to mean that there are fewer
functions of type `(ℕ → ℕ) → ℕ` (remember that the halting problem can
be fitted with this type for example). However, this means that when
we have to talk about what functions exist for order 3, we can make
assumptions about our input. No one is going to give us a solution to
the halting problem, we can capitilize on that.

In this (admittedly already rambly) post, I'd like to explore what
things become available to us, and for what reasons, if we fully
embrace intuitionism. Not merely as the *lack* of certain principles,
but rather as the existence of properties which contradict things we
otherwise take for granted.

## Continuity, A First Cut

Before we begin I'm obligated to give some idea of what I mean when I
say "continuous function" because the traditional definition appears
for functions `ℝ → ℝ` appears to be entirely irrelevant. As a
reminder, most people meet continuity as the expression

     ∀ε > 0. ∃δ > 0. ∀a b. ∣a - b∣ < δ ⇒ ∣f(a) - f(b)∣ < ε

In words, some function `f` is continuous if "wiggling" the output in
either direction ε means we only need to move the input by δ. In
particular, this implies that our function can not make any jumps. If
it did, say it jumped from 0 to 1 at 0, then we can pick `ε = 0.5` and
suddenly there's no `δ > 0` so that everything within that range is
within `0.5`! To generalize this definition we have a couple
choices. We could try and generalize a notion of distance from `∣a -
b∣` to something more general. This leads us to metric spaces but it's
not clear how to make all programs into objects in a metric space (it
can be done, but the results don't yield the definition I want for
continuity). Instead we'll go for generalizing the notion of
"close". Rather than having it be "everything within a certain
distance" we'll instead make it depend on the collection of things
we're looking at. For each set of programs we're working with, we'll
define a family of sets `τ` (the topology on our set X) and say that a
function `f : (U, τ) → (V, σ)` is continuous when

    ∀ U ∈ σ. ∃ V ∈ τ. f(V) = U

## Bar Induction
## Fan Theorem
## Continuity Theorem
## Agnosticism to Free Choice Sequences
## Conclusion
