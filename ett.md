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
run with. `⊢` is much better behaved proof-theoretically, it doesn't
immediately remove decidability either which is a nice plus. If we're
OK with giving up decidability and some nice interpretations of our
theory than `J (J)` is much more powerful. In any case, for our
purposes it's enough just to recognize that these two forms of
judgment are very different.

## Equality Reflection

So first off, the rule that we're considering here is

    M : A = B
    —————————
      A ≡ B

The equality on top is propositional equality, it's something internal
to the type theory. The equality on the bottom is the metatheoretic
notion of equality primitive to the system. Now in some type theories
this rule is completely admissible, that is `A ≡ B (M : A = B)`
holds. This is because `A = B` has a single occupant:
`refl : A = A`. Therefore, if we invert on the fact that `M : A = B`
then it must be the case that


## Wrap Up
