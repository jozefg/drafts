---
title: Computational Type Theory for Intensional Folks
tags: types
---

For context, I've been working on JonPRL for a few weeks now and thus
have spent a lot of time thinking about a system built on top of
Computational Type Theory (CTT). However, CTT has very few concise
explanations that are

 1. Formalizable
 2. Concise
 3. Written by me (got to weed out the rest)

So I figured I could create something which fits those constraints. In
particular, I'd like to try to contrast where we diverge practically
and morally from things I know better: intensional type theory (ITT).

## The Metatheory

We need to formalize a metatheory in order to discuss formalizing a
type theory. In this system need some way to discuss *judgments* and
*proofs of them*. It's the bedrock that we build everything on. In
this post I'll to use Twelf as our metatheory.

Here we formalize a judgment as a type family : `foo : bar -> baz ->
type`. And proofs become terms which are members of that type
family. The goal here is that I'll also keep an English explanation
going along side the code so even if you don't care about Twelf you
can read this.
