---
title: A Note on ETT
tags: types
---

A while ago I was talking to someone at CMU. My question went along
the lines of "Why is equality reflection such a controversial rule?"
They're response was basically "it's really not, it's just people talk
past each other by answering the wrong question". After about two
months I think I understand this response well enough to write it down
properly.

## Hypothetical Judgments

Before I can properly talk about equality I need to point out some
background. In type theory we formalize our discussion using
judgments. A judgment is simply an object of knowledge we may or may
not know. In type theory we're interested in judgments expressing
things like "This program has this type" or "These two programs are
equal when considered at this type".

On particularly important feature of our system of judgments is the
ability express some sort of implication relation between
judgments. This is called a hypothetical judgment. There are two ways
of writing this, either `J ⊢ J` or `J (J)`. However there's much more
than a notational difference here. The first one is usually used to
indicate a "uniform hypothetical judgment". Knowing `J₁ ⊢ J₂` means we
have some proof of `J₂` with a hole in it, we just need to slot in a
proof of `J₁` and it's complete. This "proof-with-hole" interpretation
means that each time that we "apply" `J₁ ⊢ J₂` the resulting process
treats the proof of J₁ without inspecting it.

On the other hand there's `J₁ (J₂)`. We're allowed to conclude this if
we just have some way of converting J₂'s proof into J₁. This is a much
more liberal mandate, if `J₂` is impossible to construct say, then `X
(J₂)` will always hold. This is not necessarily the case with `⊢`
because a lack of ways to construct doesn't mean we have a rule
capitalizing on this. For example the original thrust of homotopy type
theory was based on the observation that type theory didn't preclude
the existence of univalence and new occupants of `=`, even if they
weren't their originally. These observations simply aren't true if we
start with `J (J)` as our hypothetical judgment because we can inspect
the construction of a proof in order to build up another.

There are various trade-offs between which hypothetical judgment we
run with. `⊢` is much better behaved proof-theoretically; it doesn't
immediately remove decidability either which is a nice
plus. Additionally, `⊢` is stable under arbitrary extensions to the
system. If we're OK with giving up decidability and some nice
interpretations of our theory than `J (J)` is much more powerful. In
any case, for our purposes it's enough just to recognize that these
two forms of judgment are very different.

## Equality Reflection

So first off, the rule that we're considering here is

    M : A = B
    —————————
      A ≡ B

The equality on top is propositional equality, it's something internal
to the type theory. The equality on the bottom is the metatheoretic
notion of equality. The question of whether this rule is admissible
usually boils down to whether evidence of `M : A ≡ B` entails that `A
≡ B`. The notion of entailment is something we model using our chosen
hypothetical judgment for our system.

My main point to make here is that if you pick `J (J)` as your
hypothetical judgment then this rule is blindingly obvious. There's
(presumably) only one way to prove that `M : A = B` holds, and that's
using `refl : A = A`. So to make this rule admissible we just examine
our evidence that `M : A = B` holds as we can conclude that `A ≡ B`
must hold. This reasoning is basically equivalent to saying "there's
no way to invoke this rule where `A ≡ B`" doesn't hold according to
our system. This means that in a type theory with `J (J)` as it's
hypothetical judgment then equality reflection isn't a rule or axiom
we add to our system, it's just inversion! Most type theories, like
CiC or something, opt for `⊢` instead. This prevents us from doing
that "inversion" like process I described above and so the equality
reflection rule isn't evident. Furthermore, adding equality reflection
mixes in just a little bit of materialistic entailment. This one small
addition though has a dramatic impact on the system though! In short,
all the downsides of `J (J)` are included in our system even though we
only get one specific fraction of the resulting reasoning power. We
lose the decidability of type checking for example.

ETT is the result of taking a type theory with `⊢` like CiC or most
versions of MLTT and adding equality reflection. It's a strange hybrid
thing that is not quite as well behaved as CiC or MLTT but not as
expressive as computational type theory or something. In fact, from
the perspective of CTT or like-minded type theories asking whether or
not we have equality reflection is really the wrong question. The
interesting questions is whether or not we allow for arbitrary
materialistic entailment or not. It's really this decision which is
the fundamental one and equality reflection is just a side effect.

As a final point, whether or not you have equality reflection you'd
really always expect that propositional equality and judgmental
equality ought to be the same. In type theory though it's often the
case that we allow ourselves this "illusion" of existing in a much
larger universe than we do. In MLTT (the ⊢ one) univalence is
perfectly compatible because we don't allow ourselves to draw
completely tight limits around what propositional equality
contains. Phrased differently, while the standard model of MLTT just
has `refl` as an inhabitant of `=`, there are other models which
allows a bunch of other stuff. On the other hand, actually looking
only at nonstandard models can make the operational semantics
difficult to translate: they were only designed with the standard
model in mind. This explains part of the push behind cubical type
theory where we simply define a much richer notion of judgmental
equality which could account for univalence. In computational type
theory the limits are crisper around what `=` contains, but CTT is
built on a computation system and we deliberately leave the boundaries
of *that* fuzzy. We're not allowed to assume that all things in the pi
type are lambdas (Church Law) for example because there can be some
other crazy thing which behaves similarly under application but does
something we could never accomplish. These restrictions though
actually give us *more* power at higher types! This is the thrust of
why the lambda calculus and turing machines needn't be equivalent in
computational power at higher types.

## Wrap Up

This post is mostly an attempt to write down my thoughts lately on how
computational type theory fits in to my existing conceptions on type
theory. Now all that's left to write about is the role of continuity
in CTT :)
