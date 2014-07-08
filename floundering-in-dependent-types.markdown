---
title: Starting with Dependent Types
---

For the past 20 months I've been floundering in depedently typed
land. Not that it's the worst thing in the world of course, I've
learned so much from it.

I think that it might be worth writing down
some of my experiences to shorten someone's floundering from 20 months
to maybe only 14.

The targt audience for this post is someone
who's already decided that dependent types are interesting but isn't
sure where to start.

## Prerequisites

It's worth being up front about this. **I don't think you can jump
easily from Python to writing dependently typed code**. Sorry.

Writing a dependently typed language is the confluence of a lot of
unfortunate factors for beginners

 - [Almost Always] Purely Functional
 - Fancy Types
 - Academic Level Documentation ("Docs? What docs? Just read the
   paper and attached code!")
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
some of my favourites being Coq, Agda, Twelf, and Idris. There are a
few others that see wide spread usage but I use these four enough to
say something intelligent about them.

Which to start with is a bit nontrivially at the moment. Coq has the
most material aimed at beginners, but boasts the most baggage. Agda is
probably the simplest language but has almost no tooling.

So I'd suggest starting with either Coq or Idris. Coq simply because
it has a lot of excellent literature around it. Idris because it's
a relatively new tool that has a lot of promise of marrying practical
programming with dependent types.

### Coq

I started with Coq and I'm all to aware of the fact that it's not
always beginner friendly. It is however one of the most widely used
theorem provers and has some nice tooling including a build system,
IDEs, and documentation tool.

First off, subscribe to coq-club. It's a fairly active mailing list
that is the best (only?) place to get answers to questions. Even if
you don't post anything I learn a lot from just lurking and reading
what intelligent people are saying.

Next I'd suggest investing in some books,

 - [Certified Programming with Dependent Types](http://adam.chlipala.net/cpdt/)
 - [Software Foundations](http://www.cis.upenn.edu/~bcpierce/sf/current/index.html)

SF is a small, step by step approach to theorem proving and dependent
types in general. CPDT is a much more fast based introduction to
actually writing real world proofs.

I'd suggest getting both. I bounced back in forth between the two and
I think that really helped it click for me. CPDT is definitely a must
read if you want to read a lot of Coq code but SF foundations cemented
my admittedly weak grasp of dependent types.

The hardest thing for me to grasp about Coq was the conglomeration of
different sub-languages and how they interacted. In Coq there are
really three languages

 - Term language - A sort of mini-ML
 - Vernacular - The set of commands that instruct Coq what to do
 - Ltac - A sort of logic language for proof search

I didn't immediately grasp that these were separate, almost orthogonal
entities and a fair amount of pain ensued.

The next biggest hurdle was that Coq's error message just suck. I use
GHC daily for the last 5 years and I've never gotten error messages as
bad as with what I get with Coq. Especially when I was beginning Coq's
error messages made me very sad. Sticking with a book does help here
as CPDT in particular makes a point to address some error messages Coq
barfs at you.

Finally, knowledge with Coq is cumulative. Don't try to jump into
using more advanced features (Modules, type classes, setoids) without
first grasping more fundamental features. Especially since it seems
that the newer the feature, the more opaque the error messages. This
is a little trickier than it sounds because Coq will occasionally be
"helpful" and suggest using a setoid for example, don't do it. At
least, not right away.

### Idris

Idris is a better playground for learning about *programming* with
dependent types. Coq is a workhorse for theorems but pretty lousy for
writing fizz-buzz with. In particular, Coq's mini-ML is not the most
pleasant platform for writing large programs with, but Idris is a
fully featured programming language similar to Haskell.

In fact, if you already are a dyed-in-the-wool Haskeller, I think
you'll find Idris quite similar and very pleasant.

With decidedly mixed feelings, I can say that it's pretty obvious
where to start with Idris! The fact that it's a new language means
that it doesn't have that much written about it.

The
[documentation](http://eb.host.cs.st-andrews.ac.uk/writings/idris-tutorial.pdf)
has a nice official tutorial. In addition to this, Edwin Brady (the
language creator) has been flying about producing videos at a
tremendous rate. I'd recommend [watching][video1] a [few][video2] of
video3.

In addition to Edwin Brady, some other people produce some really nice
Idris videos. I'll link to
[Brian McKenna](http://www.youtube.com/user/pufuwozu)
and
[David Chriastansen](http://www.youtube.com/channel/UCsON_8vogp4nCQFTnfu43kA).

I discovered most of these links by hanging out around
[/r/idris](http://www.reddit.com/r/idris) so if you're a redditer, be
sure to subscribe!

## A Dash of Type Theory

I started learning about Coq with absolutely no knowledge of type
theory (TT). I still stand by the idea that you can learn and be somewhat
productive in DT languages without TT. It's pretty impossible to avoid
the conclusion that having even a little bit of knowledge can really
boost your productivity though!

One obvious benefit is you can read papers that come out on your
theorem prover of choice. At this point, DT languages are still
embedded in academia so this is a very valuable theory.

If you're feeling a bit adventures than I'd recommend looking at
either Ben Pierce's [Types and Programming Languages][TAPL]
or Bob Harper's
[Practical Foundations for Programming Languages][PFPL].
Both these books have the special privilege of being very dog-eared
and beaten up and sit on my desk; I certainly find them very useful.

## Tools

It's also worth mentioning some tools that are helpful for working
with DT programming languages.

First off, every theorem prover I've come across is Linux first,
Windows second. So I quickly found that trying to use Windows was to
constantly walk the path of "That guy who finds all those bugs on the
platform we don't test". Not the most enjoyable experience. I now only
run Linux so I don't feel this pain anymore, I'd strongly urge you to
consider using Linux.

My more controversial suggestion would be to consider switching to
Emacs. Every theorem prover I've come across is has a great Emacs IDE
and a pretty crappy interface everywhere else. There are notable
exceptions, like Idris, but if you want to write Coq Emacs + Proof
General is just staggeringly better. There's a good bit of Emacs
evangelism and tutorials already out there so I won't bore you with it here.

## Wrap Up

As a quick summary

 - Start with Coq or Idris
 - Buy books and watch talks by Smart People
 - Lurk in areas populated by Smart People
 - Invest in a bit of type theory
 - Use popular tools, Linux & Emacs

Above all, remember that this is a big leap from most programming that
you're probably used to. It took me a *long* time before I'd call
myself anything close to comfortable with Coq and a lot longer before
I felt like I could prove something significant on my own.

Give it time and don't give up, it's really worth it in the end.

Good luck.

[video1]: http://www.youtube.com/watch?v=O1t4xJzrOng
[video2]: http://www.youtube.com/watch?v=vkIlW797JN8
[TAPL]:   http://www.cis.upenn.edu/~bcpierce/tapl/
[PFPL]:   http://existentialtype.wordpress.com/2012/12/03/pfpl-is-out/
