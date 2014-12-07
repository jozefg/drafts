---
title: Treating Programs like Vending Machines
tags: haskell, types
---

Proving things about programs is quite hard. In order to make it
simpler, we often lie a bit. We do this quite a lot in Haskell when we
say things like "ignoring bottom" or "assuming everything
terminates". Most of the time, this is alright. We sometimes need to
leave the universe of terminating things, though, whenever we want to
prove things about streams or other infinite structures.

In fact, once we step into the wondrous world of the infinite we can't
rely on induction anymore. This is quite a serious issue since
induction was one of our few solid tools for proof. We can replace it
though with a nifty trick called bisimulation which gives rise to
coinduction.

## Vending Machines

Before we get to proving programs correct, let's start with proving
something simpler. The equivalence of two simple machines. These
machines (A and B) have 3 buttons. Each time we push a button the
machine reconfigures itself. A nice real world example of such
machines would be vending machines. We push a button for coke and out
pops a (very dusty) can of coke and the machine is now slightly
different.

Intuitively, we might say that two vending machines are equivalent if
and only if our interactions with them can't distinguish one from the
other. That is to say, pushing the same buttons on one gives the same
output and leaves both machines in equivalent states.

To formalize this, we first need to formalize our notion of a vending
machine. A vending machine is a comprised set of states. These states
are connected by arrows labeled with a transition. We'll refer to the
start of a transition as its domain and its target as the
codomain. This group of transitions and states is called a labeled
transition system (LTS) properly.

Let's consider a relation `R` with `A R B` if and only if

 1. There exists a function `f` from transitions from A to transitions
    from B so that `x` and `f(x)` have the same label.
 2. Further, if `A R B` and `A` has a transition `x`, then the
    codomain of `x` is related to the codomain of `f(x)`.
 3. There is a `g` satisfying 1. and 2., but from transitions from B
    to transitions from A.

This definition sets out to capture the notion that two states are
related if we can't distinguish between them. The fancy term for such
a relation is a bisimulation. Now our notion of equivalence is called
bisimilarity and denoted `~`, it is the union of all bisimulations.

Now how could we prove that `A ~ B`? Since `~` is the union of all
bisimulations, all we need to is construct a bisimulation so that
`A R B` and hey presto, they're bisimilar.

TODO EXAMPLE PROOFS

## From Vending Machines to Programs

It's all very well and good that we can talk about the equality of
labeled transition systems, but we really want to talk about programs
and pieces of data. How can we map our ideas about LTSs into programs?

Let's start with everyone's favorite example, finite and infinite
lists. We define our domain of states to be

    L(A) = {nil} ∪ {cons(n, xs) | n ∈ ℕ ∧ xs ∈ A}

We have to define this as a function over `A` which represents the
tail of the list which means this definition isn't recursive! It's
equivalent to

``` haskell
    data ListF a = Cons Int a | Nil
```

What we want here is a *fixed point* of `L`, an element `X` so that
`L(X) = X`. This is important because it means

``` haskell
    cons :: ℕ → X → L(X)
    cons :: ℕ → L(X) → L(X)
```

Which is just the type we'd expect cons to have. There's still a snag
here, what fixed point do we want? How do we know one even exists? I'd
prefer to not delve into the math behind this (see TAPL's chapter on
infinite types) but the gist of it is, if for any function `F`

 1. `F` is monotone so that `x ≤ y ⇒ F(x) ≤ F(y)`
 2. `F` is cocontinuous so that `∩ₓF(x) = F(∩ₓ)`

Then there exists an `X = F(X)` which is greater or equal to all other
fixpoints. The proof of this isn't too hard, I encourage the curious
reader to [go and have a look][knaster-tarksi].

This greatest fixed point what we get with Haskell's recursive types
and that's what we want to model. What's particularly interesting is
that the greatest fixed point includes infinite data which is very
different than the least fixed point which is what we usually prefer
to think about when dealing with things like F-algebras and proofs by
induction.

Now anyways, to show `L` has a fixed point we have to show it's
monotone. If `X ⊆ Y` then `L(X) ⊆ L(Y)` because `x ∈ L(X)` means
`x = nil ∈ L(Y)` or `x = cons(h, t)`, but since `t ∈ X ⊆ Y` then
`cons(h, t) ∈ Y`. Cocontinuity is left as an exercise to the reader.

So `L` has a greatest fixed point: `X`. Let's define an LTS with
states being `L(X)` and with the transitions `cons(a, x) → x` labeled
by `a`. What does bisimilarity mean in this context? Well `nil ~ nil`
since neither have any transitions. `cons(h, t) ~ cons(h', t')` if and
only if `h ~ h'` and `t ~ t'`. That sounds a lot like how equality
works!

Demonstrate this let's define two lists

``` haskell
    foo = cons(1, foo)
    bar = cons(1, cons(1, bar))
```

Let's prove that `foo ~ bar`. Start by defining a relation `R` with
`foo R bar`. Now we must show that each transition from `foo` can be
matched with one from `bar`, since there's only one from each this is
easy. There's a transition from `foo → foo` labeled by `1` and a
transition from `bar → cons(1, bar)` also labeled by one. Here lies
some trouble though, since we don't know that `foo R cons(1, bar)`,
only that `foo R bar`. We can easily extend `R` with
`foo R cons(1, bar)` though and now things are smooth sailing. The
mapping of transitions for this new pair is identical to what we had
before and since we know that `foo R bar`, our proof is finished.

To see the portion of the LTS our proof was about

     foo                  bar
      | 1                  | 1
     foo             cons(1, bar)
      | 1                  | 1
     foo                  bar

and our bisimulation `R` is just given by
`{(foo, bar), (foo, cons(1, bar))}`.

Now that we've seen that we can map our programs into LTSs and apply
our usual tricks there, let's formalize this a bit.

## A More Precise Formulation of Coinduction

First, what exactly is [co]induction? Coinduction is a proof principle
for proving something about elements of the greatest fixed point of a
function, `F`. We can prove that the greatest fixed point, `X`, is the
union of all the elements so that `F(X) ≥ X`.

If we can prove that there exists an `X ≤ F(X)` then we know that
`X < gfp(F)`. That is the principle of coinduction. It also should
look very similar to how we proved things about `~`.

## Dithering about Duality

## [Finally] Some Code

## Wrap Up

[knaster-tarksi]: https://en.wikipedia.org/wiki/Knaster%E2%80%93Tarski_theorem
