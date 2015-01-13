---
title: Notes on Modal Logic
tags: types, notes
---

One particularly nifty thing I've been reading about lately is modal
logic. This is a family of logics that talk about different "modes" of
truth.

One way to picture this is a particular modal logic is like a set of
world. Each of these worlds has it own set of rules and
judgments. Some of these worlds subsume others, so it makes sense that
if `A true` in world `P`, then `A* true` in `Q`.

In a more concrete sense, we usually talk about 3 particular worlds in
modal logic. First is the normal truth we are used to. Second is □,
read "necessary". This is the strictest world, in it is the set of all
things that are "necessarily true" and don't depend on normal
hypothetical context. Finally there is ♢ or possibility. This is the
opposite of □, all things that are true are possible, but possible
things may depend on other possible hypotheses.

Modal logic provides a framework for us to discuss these
interdependent worlds and make statements and proofs in them.

## Rules for A Simple Logic

To start with let's describe how the modal logic we described above
(with □ and ◯) would look.

First we have the syntax

    A ::= □ A
       |  ◯ A
       |    ⊤
       |    ⊥
       |  A ∧ B
       |  A ∨ B
       |  A ⇒ B
    Γ ::= • | Γ, A true | Γ, A valid
    Δ ::= • | Γ, A valid

So we have ◯ and □ as "modalities" that modify propositions and then a
normal logical connectives. Let's define the rules for the judgments

     Δ; Γ ⊢ A true
     Δ; Γ ⊢ A valid
     Δ; Γ ⊢ A lax

simultaneously. I'll shorten `A true` to `A`.

    ————————                  Δ; • ⊢ A
    Δ; Γ ⊢ ⊤               ——————————————
                           Δ; Γ ⊢ A valid
    Δ; Γ ⊢ A    Δ; Γ ⊢ B
    ————————————————–——–     Δ; Γ ⊢ A
       Δ; Γ ⊢ A ∧ B        —————————————
                           Δ; Γ ⊢ A poss
      Δ; Γ ⊢ A
    ————————————           Δ; Γ ⊢ A valid
    Δ; Γ ⊢ A ∨ B           ——————————————
                             Δ; Γ ⊢ □ A
      Δ; Γ ⊢ B
    ————————————           Δ; Γ ⊢ A poss
    Δ; Γ ⊢ A ∨ B           —————————————
                             Δ; Γ ⊢ ◯ A
     Δ; Γ, A ⊢ B
    ————————————
    Δ; Γ ⊢ A ⇒ B

The only interesting introduction rules are the four on the right, a
valid proposition can be introduced if and only if it doesn't depend
no the context of `true` and `poss` things. A possible thing can be
introduced by a true thing. Finally, □ and ◯ are just reflections of
the `poss` and `valid` judgments into the `true` one.

For elimination rules

                                 Δ, A valid; Γ ⊢ C
    ———————————                  —————————————————
    Δ; Γ, ⊥ ⊢ C                    Δ; Γ, □ A ⊢ C

    Δ; Γ, A ⊢ C    Δ; Γ, B ⊢ C       Δ; Γ ⊢ A
    ————————————————–——–——————   ——————————————
         Δ; A ∨ B, Γ ⊢ C          Δ; Γ ⊢ A poss

    Δ; Γ, A, B ⊢ C                 Δ; Γ, A ⊢ A poss
    ———————————————              ———————————————————
    Δ; Γ, A ∧ B ⊢ C               Δ; Γ, ◯ A ⊢ A poss

      Δ; Γ, □ A ⊢ C              Δ; Γ ⊢ A poss
    —————————————————            —————————————
    Δ, A valid; Γ ⊢ C              Δ; Γ ⊢ ◯ A


Once again pay attention to the right column and the one on the bottom
left. First off we can freely convert between the truth of the
`□ A` and `A valid`. Possibility is a bit odd, when trying to prove
something is possible we may freely convert `◯ A` to just `A`! This

## Kripke Semantics
## Adjoint Logic
## Wrap Up
