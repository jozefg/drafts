---
title: Notes on Proof Theory: Part 2
tags: types
---

Hi again. In the last [post][last-post] we talked a bit about the
natural deductions and proved a few theorems about it. Mostly, we
talked about local soundness and completeness. One thing that bothered
me a bit was the "local" prefix. We can prove that the elimination
rules of our system balance out, but what about the system as a whole?
How can we guarantee properties about it?

If we want to state something like local soundness and completeness on
a global scale, we first need to figure out what the equivalent of a
"reduced" and "expanded" proofs are.

## Verifications

We want to talk about proofs that are "just applications of
introduction rules". The picture isn't quite as simple but that's the
idea.

To this end we define a new judgment `A ↑` meaning that "`A` has a
verification". For most connectives these are just the introduction
rules

```
     A ↑  B ↑
    —————————— ∧ I
      A ∧ B ↑

     A ↑  B ↑
    —————————— ∧ I
      A ∧ B ↑

     A ↑  B ↑
    —————————— ∧ I
      A ∧ B ↑

```


## Usages
## A Calculus of Verifications and Uses
## Global Soundness and Completeness


[last-post]: /posts/2015-02-11-proof-theory1.html
