---
title: Write You A Proof Assistant for Great Migraines
tags: types, math
---

Hi folks. It's been quite a while since I've written anything; mostly
due to some health issues that have been plaguing me lately. Now that
I'm a bit more on the up and up I wanted to share a talk turned
essay. This was originally going to be presented at LambdaConf but
this year LambdaConf had a but more, um, excitement than I was up to.

I'd like to talk about how one would go about writing a proof
assistant. Now this post comes with some code! You can go check out
[miniprl][miniprl] and see a small example of the ideas that I'm
talking about.

## What Do We Want Out of Our Proof Assistant

Right off the bat we have a very important question that needs
answering: what are we hoping to do with our proof assistant. If we're
looking to formalize logics, we want a different set of tools than if
we're looking to prove properties of aircraft flight. In my case, I
have a fairly modest goal. I want to be able to write a program,
specify some properties, and convince a reasonable skeptic (my laptop)
that my program actually has those properties.

In order to do this, I'll put on my type theorist hat and draw a few
equivalences here.

 - My proofs will be honest to goodness programs
 - My properties will be formulated as types
 - My proof assistant will validate that a given proof (program)
   proves a certain property (type)

Embracing this perspective races some questions those. It's clear how
we might interact with the proof assistant but for most of us types
are not exactly the high level assertions we want to say about our
programs. In fact, in most cases types just form the mundane role of
letting the compiler go-go fast by knowing some information about the
data it's manipulating. If we work with only these types we're dead in
the water. Instead I'll propose a different way of thinking about
types: types are specifiers of computation. That is, programs and
computation exist all on their own and after the fact, we may observe
that a program follows a specification and declare that it has a
certain "type".

So familiar types can be viewed in this light even though they're
slightly boring: `int` is a specification that something computes
to... a number. Thrilling no? Likewise with `string`, `char`, or
`bool` or whatever, we really just specify that the value resulting
from a computation is of a particular form.

We can think of these specification almost as a very formalized sort
of documentation. Viewed in this light, it makes sense for us to build
up a language to talk about specifications in more detail!

 - We can take `SPEC1` and `SPEC2` and "and" them together, to produce
   a new specification. A program satisfies `and(SPEC1, SPEC2)` if and
   only if it individually satisfies `SPEC1` and `SPEC2`.
 - By analogy, we can "or" two specifications. A program satisfies
   `or(SPEC1, SPEC2)` if and only if it individually satisfies either
   `SPEC1` or `SPEC2`.

Okay, so this is all well and good. But we're still woefully far away
from being able to specify `quicksort` or something. In fact, if we
poke around documentation in any programming language, we find that
most things are of the form

> Given `x` and `y` as input parameters, `foo` will return `bar`.

This suggests that we need to introduce *programs* into our
specifications. That is, that we should be able to talk about random
programs (like `x` and `y` here) and say things about them in our
specification. In fact, we can really go off the deep end here and
just make our specification an arbitrary program that *computes to the
thing that we want*.

Now since I'm a type theorist, the way I approach this is by
formulating my properties as a particular type. This type may
something very simple

[miniprl]: https://www.github.com/jozefg/miniprl
