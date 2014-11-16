---
title: Notes on Linear Logic | Lambda Calculus
tags: types
---

I've got a bowl full of vegetarian spaghetti (mmm vegetarians) so now
seems like as good a time as any to talk about a particularly useful
type of logic that not enough people know about.

## Logicians Make Terrible MBAs

In a standard logic, there's usually a rule to the effect

    A, A → B
    ————————
       B


This has a fancy name "modus ponus" and it captures the fairly mundane
notion of how we actually use `→`. We can draw all sorts fun real
world analogy

 1. If I vote for candidate A, they will win the election. I voted for
     candidate A They won the election!
 2. If I speed, I will get a ticket.  I sped on the way to the
    election.  I got a ticket (my newly elected candidate can probably
    help with this)

Now as nice a picture as it paints, this doesn't wholly represent
causality. Consider a logician standing in front of a vending
machine. There might be some axioms like

 1. `getChips : One Dollar → Chips`
 2. `getChocolate : One Dollar → Chocolate`

If our logician has proven they have a dollar with some derivation `D`
we can imagine some proof like

          D            getChips               D               getChocolate
     ———————————    ——————————————————   ———————————    ——————————————————————–
      One Dollar    One Dollar → Chips    One Dollar    One Dollar → Chocolate
     ——————–———————————————————————————  ——————–———————————————————————————————
                Chips                               Chocolate
     ————————————————————————————————————————————————————————————————————–—————
                                Chips ∧ Chocolate

And so our logician confidently inserts their dollar bill intent on
getting both chips and chocolate and promptly becomes stuck. Clearly
something was off with our thinking here, we've somehow concluded that
we can spend the same dollar twice!

The issue is that a dollar isn't a fact that we can use
repeatedly. Somehow it's something more limited, a resource we can
choose to use once and only once.

### Linear Logicians are Better MBAs

Clearly to model some ideas we need to sacrifice some expressive
power. Specifically, we need to get rid of a few problematic
structural rules.

For those not familiar with the term, a structural rule is an
inference rule that doesn't deal with a specific proposition, but
rather with the structure of the rule in general. For example, there's
a structural rule in normal logic called weakening which looks
something like

          Γ ⊢ P
        —————————
        Γ, Γ' ⊢ P

In other words, for *any* proposition if we can prove it with some set
of facts, we can prove it with those facts plus other things. This
makes perfect sense and most of the time, we don't even really mention
rules like this. However, it's sometimes interesting to discard such
axioms and see what happens. In particular, for linear logic we'll
discard the weakening above as well as another called contraction


    Γ, A, A ⊢ P
    ———————————
     Γ, A ⊢ P

This rule allows us to duplicate proofs. Again, for most uses this
seems completely trivial. By discarding these structural rules, we've
suddenly made the number of occurrences of a hypothesis in a context
is a paramount importance!

In order to gain some intuition for this, let's go through how a few
connectives might be defined in this logic.

First, is linear implication, ⊸. Some people pronounce this lollipop.

     Γ₁ ⊢ A ⊸ B    Γ₂ ⊢ A
     —————————————————————
           Γ₁, Γ₂ ⊢ B

The first thing we notice is that we're now splitting the context. We
no longer just use the same Γ everywhere since we don't allow any
propositions to be duplicated.

To ensure that hypothesis are unused, the "leaves" of our derivations
can only happen in empty environments.

      —————————
      X, • ⊢ X

      ——————
      • ⊢ ⊤

Now this has the interesting effect. Now there's an interesting
dilemma, whether conjuction and disjunction should be *additive* or
*multiplicative*. If we split the context when we for `A ∧ B` then we
have `Γ ⊢ A ∧ B` we know we can use both `A` and `B`. If we don't
split the context then `Γ ⊢ A ∧ B` can only give us `A` or `B`. We
call the former multiplicative and notate it with `⊗` and the latter
additive and notate it with `&`.

The rules for `⊗` are

    Γ₁ ⊢ A   Γ₂ ⊢ B
    ———————————————
     Γ₁, Γ₂ ⊢ A ⊗ B

     Γ₁ ⊢ A ⊗ B   Γ₂, A, B ⊢ C
     —————————————————————————
           Γ₁, Γ₂ ⊢ C


So while we had to split the context to introduce an ⊗, we got both
`A` and `B` out of the elimination rules. Conversely with &

     Γ ⊢ A   Γ ⊢ B
     —————————————
      Γ ⊢ A & B

     Γ₁ ⊢ A & B   Γ₂, A ⊢ C
     ——————————————————————
           Γ₁, Γ₂ ⊢ C

     Γ₁ ⊢ A & B   Γ₂, B ⊢ C
     ——————————————————————
           Γ₁, Γ₂ ⊢ C


Now the introduction rule is a bit laxer, we can use the full context
for both branches. We do pay the piper with weaker elimination rules
though.

Now our previous dilemna of the dollar is solved! Without weakening we
can't reuse the proof (`D`) of the existence of a dollar so we can't
spend the same money twice.

### Mixing Linear and Non-linear Hypothesis

### Linear Lambda Calculus

### Meanwhile, in the Real World

### Wrap Up
