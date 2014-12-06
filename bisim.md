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

## Labeled Transition Systems

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
start of a transition as its domain and its target as the codomain.

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

Now it's all very well and good that we can prove the equivalence of
programs

## A Precise Formulation of Coinduction

## Relation to Induction

## [Finally] Some Code

## Wrap Up
