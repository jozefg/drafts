---
title: Not Another Dependent Types Sales Pitch
---

For the past 20 months I've been floundering in depedently typed
land. Not that it's the worst thing in the world of course, I've
learned so much from it.

I think that it might be worth writing down
some of my experiences to shorten someone's floundering from 20 months
to maybe only 14.

## Prerequisites

It's worth being up front about this. **I don't think you can jump
easily from Python to writing dependently typed code**. Sorry.

Writing a dependently typed language is the confluence of a lot of
unfortunate factors for beginners

 - [Almost Always] Purely Functional
 - Fancy Types
 - Academic Level Documentation
 - Very Few Tools

Now this might change in the next 5, 10, 15 years. In the here in now,
however, I think we'll have to accept the fact that there is enough of
a barrier that some intermediate languages are in order.

If you'd like to write some serious dependently typed code, I'd say
you should know a bit about one or more of the following

 - SML
 - OCaml
 - Haskell
 - Scala

If you've got some chops with fancier static types and functional
programming without teaching one of these 4, props to you and read on.

## Which Language

So the next obvious question to ask when learning about dependent
types is what language to pick?

There are a lot! It seems like every other university maintains their
own theorem-prover. Nowadays I'd say there are a few main players,
some of my favourites being Coq, Agda, and Idris.

Which to start with is a bit nontrivially at the moment. Coq has the
most material aimed at beginners, but boasts the most baggage. Agda is
probably the simplest language but has almost no tooling.

So I'd suggest starting with either Coq or Idris. Coq simply because
it has a lot of excellent literature around it and Idris because it's
a relatively new tool that has a lot of promise of marrying practical
programming with dependent types.

## Coq

I spent a decent chunk of my time flailing about learning Coq so I
have opinions about learning it.

First off, subscribe to coq-club. It's a fairly active mailing list
that is the best (only?) place to get answers to questions.

Next I'd suggest investing in some books,

 - [Certified Programming with Dependent Types](http://adam.chlipala.net/cpdt/)
 - [Software Foundations](http://www.cis.upenn.edu/~bcpierce/sf/current/index.html)

SF is a small, step by step approach to theorem proving and dependent
types in general. CPDT is a much more fast based introduction to
actually writing real world proofs.

I'd suggest getting both. I bounced back in forth between the two and
I think 
