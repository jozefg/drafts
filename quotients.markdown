---
title: Notes on Quotients Types
---

Lately I've been reading a lot of type theory literature. One of the
things that struck me was that a lot of this stuff wasn't complicated,
it was just hard to find that original, simple introduction. I suppose
this makes sense, no researcher wants to summarize the last 50 years
of research for a small result! It mean that I have to apply more
google-fu than I would like to understand what's going on.

In effort to help my future self, I'm going to jot down a few thoughts
on quotient types, the subject of some recent google-fu.

## But Why!

The problem quotient types are aimed at solving is actually a very
common one. I'm sure at some point or another you've used a piece of
data you've wanted to compare for equality. Additionally, that data
properly needed some work to determine whether it was equal to
another piece.

A simple example might would be representing rational numbers. A
rational number is a fraction of two integers, so let's just say

``` haskell
    type Rational = (Integer, Integer)
```

Now all is well, we can define a `Num` instance and what not. But what
about equality? Clearly we want equivalent fractions to be equal. That
should mean that `(2, 4) = (1, 2)` since they both represent the same
number.

Now our implementation has a sticky point, clearly this isn't the case
on its own! What we really want to say is "`(2, 4) = (1, 2)` up to
normalization".

Haskell's own `Rational` type solves this by not exposing a raw
tuple. It still essentially exists under the hood, but we only expose
smart constructors that will reduce our fractions as far as possible.

This is displeasing from a dependently typed setting however, we want
to be able to formally prove the equality of some things. This
"equality modulo normalization" leaves us with a choice. Either we can
really provide a function which is essentially

``` agda
    foo : (a b : Rational)
        -> Either (reduce a = reduce b) (reduce a /= reduce b)
```

This doesn't really help us though, there's no way to express that `a`
should be observationally equivalent to `b`. This is a problem
seemingly as old as dependent types: How can we have a simple
representation of equality that captures all the structure we want and
none that we don't.

Hiding away the representation of rationals certainly buys us
something, we can use a smart constructor to ensure things are
normalized. From there we could potentially prove a (difficult)
theorem which essentially states that

``` agda
    =-with-norm : (a b c d : Integer)
                -> a * d = b * c -> mkRat a b = mkRat c d
```

This still leaves us with some woes however, now a lot of computations
become difficult to talk about since we've lost the helpful notion
that `denominator o mkRat a = id` and similar. The lack of
transparency shifts a lot of the burden of proof onto the code privy
to the internal representation of the type, the only place where we
know enough to prove such things.

Really what we want to say is "Hey, just forget about a bit of the
structure of this type and just consider things to be identical up to
`R`". Where `R` is some equivalence relation, eg

  1. `a R a`
  2. `a R b` implies `b R a`
  3. `a R b` and `b R c` implies `a R c`

If you're a mathematician, this should sound similar. It's a lot like
how we can take a set and partition it into equivalence classes. This
operation is sometimes called "quotienting a set".

For our example above, we really mean that our rational is a type
quotiented by the relation `(a, b) R (c, d)` iff `a * c = b * d`.

## More than Handwaving

## Implementations (that I know of)

## Wrap up
