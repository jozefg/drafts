---
title: Notes on Proof Theory: Part 1
tags: types
---

I write a lot about types. Up until now however, I've only made
passing references to the thing I've actually been studying in most of
my free time lately: proof theory. Now I have a good reason for this:
the proof theory I'm interested in is undeniably intertwined with type
theory and computer science as a whole. In fact, you occasionally see
someone draw the triangle


               Type Theory
              /           \
             /             \
     Proof Theory ---- Category Theory

Which nicely summarizes the lay of the land in the world I'm
interested in. People will often pick up something will understood on
one corner of the triangle and drag it off to another, producing a
flurry of new ideas and papers. It's all very exciting and leads to
really cool stuff. I think the most talked about example lately is
homotopy type theory which drags a mathematical structure (weak
infinite groupoids) and hoists off to type theory!

If you read the [unprofessional, mostly incorrect, and entirely more
fun to read] blog posts on these subjects you'll find most of the lip
service is paid to category theory and type theory with poor proof
theory shunted off to the side.

In this post, I'd like to jot down my notes on Frank Pfenning's
introduction to proof theory materials to change that in some small
way.

## What is Proof Theory

The obvious question is just "What is proof theory?". The answer is
that proof theory is the study of proofs. In this world we study
proofs as first class mathematical objects which we prove interesting
things about. This is the branch of math that formalizes our handwavy
notion of a proof into a precise object governed by rules.

We can then prove things like "Given a proof that `Γ ⊢ A` and another
derivation of `Γ, A ⊢ B`, then we can produce a derivation of
`Γ ⊢ B`. Such a theorem is utterly crazy unless we can formalize what
it means to derive something.

From this we grow beautiful little sets of rules and construct
derivations with them. Later, we can drag these derivations off to
type theory and use them to model all sorts of wonderful phenomena. My
most recent personal example was when folks noticed that the rules for
modal logic perfectly capture what the semantics of static pointers
ought to be.

So in short, proof theory is devoted to answering that question that
every single one of your math classes dodged

> Professor, what exactly is a proof?

## Basic Building Blocks

In every logic that we'll study we'll keep circling back to two core
objects: judgments and propositions. The best explanation of judgments
I've read comes from Frank Pfenning

> A judgment is something we may know, that is, an object of
> knowledge. A judgment is evident if we in fact know it.

So judgments are the things we'll structure our logic around. You've
definitely heard of one judgment: `A true`. This judgment signifies
whether or not some proposition `A` is true. Judgments can be much
fancier though: we might have a whole bunch of judgments like
`n even`, `A possible` or `A resource`.

These judgments act across various syntactic objects. In particular,
from our point of view we'll understand the meaning of a proposition
by the ways we can prove it, that is the proofs that `A true`.

We prove a judgment `J` through inference rules. An inference rule
takes the form

    J₁ J₂ .... Jₓ
    —————————————
         J

Which should be read as "When `J₁`, `J₂` ... and `Jₓ` hold, then so
does `J`". Here the things above the line are premises and the ones
below are conclusions. What we'll do is define a bunch of these
inference rules and use them to construct proofs of judgments. For
example, we might have the inference rules

                 n even
     ——————    ————————————
     0 even    S(S(n)) even

for the judgment `n even`. We can then form proofs to show that `n
even` holds for some particular `n`.


           ——————
           0 even
        ————————————
        S(S(0)) even
     ——————————————————
     S(S(S(S(0)))) even

This tree for example is evidence that `4 even` holds. We apply
second inference rule to `S(S(S(S(0))))` first. This leaves us with
one premise to show, `S(S(0)) even`. For this we repeat the process
and end up with the new premise that `0 even`. For this we can apply
the first inference rule which has no premises completing our proof.

Now we want to define logical systems with these inference rules. For
example, we might want to give meaning to the proposition `A ∧ B` (A
and B). To do this we define its meaning through the inference rules
for proving that `A ∧ B true`. In this case, we have the rule

    A true  B true
    —————————————— (∧ I)
      A ∧ B true

I claim that this defines the meaning of `∧`: to prove a conjunction
to be true we must prove its left and right halves. The rather
proof-theoretic thing we've done here is said that the meaning of
something is what we use to prove it. This is sometimes called the
"verificationist perspective". Finally, note that I annotated this
rule with the name `∧ I` simply for convenience to refer it.