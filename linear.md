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
seems completely trivial. By discarding

### Mixing Linear and Non-linear Hypothesis

### Linear Lambda Calculus

### Meanwhile, in the Real World

### Wrap Up
