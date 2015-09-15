---
title: Two Different Flavors of Type Theory
tags: types
---

So summer seems to be about over. I'm very happy with mine, I learned
quite a lot. In particular over the last few months I've been reading
and fiddling with a different kind of type theory than I was used to:
computational type theory. This is the type theory that underlies
Nuprl (or [JonPRL][jonprl] cough cough).

One thing that stood out to me was that you could do all these
absolutely *magical* things in this system that completely flew in the
face of what I was used to after 3 or so years of Coq and Agda. In
this post I'd like to sketch some of the philosophical differences
between CTT and a type theory more in the spirit of CiC. I'd like to
introduce some folks to the other side of the aisle and write down the
solutions to some misconceptions I've had for a while.

## Formal Type Theory and Props-as-Types #1

First things first, let's go over the more familiar notion of type
theory. To develop one of these type theories you start by discussing
some syntax. You lay out the syntax for some types

    A ::= Σ x : A. A | Π x : A. A | ⊤ | ⊥ | ...

And now we want to describe

... lots of stuff happens ...

But these terms that we've written down aren't really
programs. They're just serializations of the collections of rules
we've applied to prove a proposition. There's no ingrained notion of
"running" an `M`. What we have instead is this `≡` relation which just
specifies which symbols we consider equivalent. There's no reason we ≡
needs to be a reasonable term rewriting system or anything. If we're
good at our jobs it will be, sometimes (HoTT) it's not completely
clear what that computation system is even though we're working to
find it.

So that's formal type theory, it's a more syntactic thing where one
exhaustively specifies rules for `M : A` and `M ≡ N : A`. This type
theory is a formal system like any other and it's useful for a lot of
things. Our primary concern here was to pick rules with good
properties like soundness, decidability, ability to prove X
proposition, and so on. A secondary concern that comes *after*
formalizing what terms exist and they're types is what sorts of proofs
we want to identify as equal and even on this we impose the same
decidability requirements. It's not the case that it's any harder to
prove that `M ≡ N : A → B` then `M ≡ N : ⊤` because it's all the same
decidable judgment.

*It was pointed out to me that there is a very large gap between
 decidable and efficiently computable. Many, many things in formal
 type theory are inefficient to compute*.
