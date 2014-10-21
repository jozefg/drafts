---
title: Notes on Focusing
tags: types
---

I've been spending a lot of time whacking my head on focusing
literature. I'm stuck on this train for a couple hours so have an
accumulation of my thoughts.

### What Is Focusing

In a nutshell, focusing is a strategy to create proofs that minimizes
the amount of choices available at each step. Focusing is thus
amenable to mechanization since a computer is very good at applying a
lot of deterministic procedures but incredibly bad at nondeterministic
choice.

Now when we set out to define a focused system we usually do something like

 1. Formalize our logical framework with natural deduction
 2. Translate our framework into a sequent calculus
 3. Transform our sequent calculus into a focused variant

At each of these steps there's a proof that says something like
"System 2 is sound and complete with respect to System 1". We can then
chain these proofs together to get that we can transform any
nonfocused proof into a focused one (focalization) and the reverse
(de-focalization).

In order to actually carry out these proofs there's a fair amount of
work and pain. Usually we'll need something like cut elimination and
identity expansion.

### Groundwork

Now before we go on to define an example logic, let's notice a few
things about logical. First off, in sequent calculus there are left
and right rules. Left rules decompose known facts into other known
facts while right rules transform our goal. There's also an identity
sequent which more or less just states

     A is an atom
     —————————————
       Γ, A → A

This is a bit boring though, so we'll circle back to it later.

Now certain rules are invertible: their conclusion implies their
premise in addition to the reverse. For example if I said you must
prove `A ∧ B` clearly we'll have to prove both `A` and `B` in order to
prove `A ∧ B`; there's no alternative set of rule applications that
let us circumvent proving `A` and `B`.

This means that if we were mechanically trying to prove something of
the form `A ∧ B` we can immediately apply the right rule that
decomposes `∧` into 2 goals.

We can these sort of rules invertible or asynchronous. Dually, there
are rules that when applied transform our goal into something
impossible to prove. Consider `⊥ ∨ ⊤`, clearly apply the rule that
transforms this into `⊥` would be a bad idea!

Now if we begin classifying all the left and write rules we'll notice
that the tend to all into 2 categories

 - Things with invertible left rules and noninvertible right rules
 - Things with noninvertible left rules and invertible right rules

We dub the first group "positive" things and the second "negative"
things. This is called polarization and isn't strictly necessary but
greatly simplifies a lot of our system.

Now there are a few things that could be considered both positive and
negative. For example we can consider `∧` as positive with

      Γ → A⁺  Γ → B⁺
     ———————————————
       Γ → A⁺ ∧ B⁺

       Γ, A⁺, B⁺ → C
     —————————————————
       Γ, A⁺ ∧ B⁺ → C

In this case, the key determiner for the polarity of ∧ comes from its
subcomponents. We can just treat ∧ as positive along with its
subcomponents and with an appropriate dual ∧⁻, our proof system will
still be complete.

As a quick example, implication `⊂` is negative. the right rule

     Γ, A → B
    ——————————
    Γ → A ⊃ B

While its left rule isn't

     Γ, A ⊃ B → A  Γ, B, A ⊃ B → C
     ——————————————————————————————
             Γ, A ⊃ B → C

Since we could easily have something like `⊥ ⊃ ⊤` but using this rule
would entail (heh) proving `⊥`! Urk. If our system applied this rules
remorselessly, we'd quickly end up in a divergent proof search.x

## An Actual Focused System

Now that we've actually seen some examples of invertible rules and
polarized connectives, let's see how this all fits into a coherent
system.

Our system is broken up into 3 judgments. The first is `Γ; Ω ⊢ A`. `Ω`
is an ordered list of things which we might invert upon. In this
judgment we basically apply as many invertible rules as many places as
we can.

     Γ, A⁻; Q ⊢ U
    ——————————————
    Γ; ↓A⁻, Q ⊢ U

## Proving This

## Wrap Up
