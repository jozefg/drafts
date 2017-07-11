---
title: What's the Deal with Register Allocation
tags: compilers
---

Hi folks, it's been a while since I've written everything but today I
wanted to talk about a subject which is both extremely cool and
extremely confusing: register allocation. For a long time, this was
the last "mysterious" part of compilation for me and I never found a
nice, simple, explanation of coloring-based register allocation to
explain how this all kind of shakes out. This post is my too late
attempt to help past-me understand how that works.

## What Problem Are We Solving?

First off, register allocation is a phase in a compiler. As far as
phases go, it comes fairly late, after much of the language has been
rendered unrecognizable by previous compilation passes and
intermediate languages. It's job is to remove the abstraction of
"infinitely many variables". After all, we want spit out assembly and
once we get to code of the form

    L0:
    t3 = t1 + t2
    t2 = t1
    t1 = t3
    if t2 < 100 goto L0
    call printf "%d\n" t2

we're pretty damn close! It seems like we can compile each of these
lines into one or two well chosen assembly instructions with references
to some runtime functions (all hail `printf` magic), maybe stick a few
things like string fragments elsewhere and we'll be all set!

But we have this lingering issue: what are `t1`, `t2` and `t3`? We
obviously mean them to be temporary slots of data that we can act on
with assembly instructions like `add` or something but we don't have a
lot of those on some machines and we probably will have quite a lot of
temporaries, especially if we've used SSA or something like
it. Register allocation is the pass where we walk our code and compute
a mapping from temporary names like `t3` to actual machines registers
like `%eax`. If we can't fit everything into memory, so be it, then
register allocation is also the phase where we may rewrite code to
insert load and store instructions as appropriate to store temporaries
on the stack for a little bit to get rid of them.

## How Do We Do This?

This seems like kind of a hard problem in general, we need to figure
out when we can schedule different temporaries into different
registers. This means we need to figure out answers to questions like

 1. Do I need this temporary in the future, or can it be permanently
    overwritten?
 2. Can I schedule `a` and `b` to both be stuck in `r` or will I need
    them at the same time?
 3. I use `a` in a tight loop, how can I ensure that it goes in a
    register before `b` which I'm just checking at the beginning and
    end of the function?

All of these demand some form of liveness analysis to construct a
critical data structure known as an interference graph. For us, this
is just a graph that has temporaries as nodes and an edge in the graph
indicates that at some point, we need the two temporaries to be
accessible (live) at the same time. So for example, in the code

    a = 1
    b = 1
    c = a + b
    d = b + c

We could construct an interference graph of

    a ---- b
    |      |
    |      |
    c------d

Notably, there's no node from `a` to `d` because `a` and `d` do not
need to be alive at the same time, but there is one from `b` and `a`
because both need to be alive so that we can compute `c` on
line 3. Constructing the interference graph is the first major hurdle
in performing register allocation and for it, we'll use a classic
dataflow trick: a work list.

After we have a liveness graph, we're in a position to make a really
wonderful simplification of our problem: let's switch from register
allocation to graph coloring. That is, choosing `k` colors so that
each node in our graph gets a color and no two neighbors have the same
color. This is a classic NP-complete problem but it's very well
understood. If we can come up with a k coloring of our graph, we just
translate those k colors into our k registers and we're done!

Of course, we can always choose k = 1 and we'll get stuck so clearly
our algorithm won't just be returning a coloring, it will also
potentially have a list of nodes it couldn't color successfully. For
this, we propose the simple loop

 0. Build our interference graph with liveness analysis
 1. Try to color our graph
 2. If we can, stop
 3. Else, take the list of nodes we couldn't color and rewrite the
    program so we lazily load them off the stack and store them back
    immediately afterwards
 4. goto 1

This process will terminate so long as there are enough registers to
hold each instructions arguments (and of course there will be). This
isn't quite the register allocator I want, I'd like to convert
eliminate almost all `mov` instructions with something called
coalescing but I don't want to bite off more than I can chew for this
post.

## The Language We're Using

## Liveness Analysis
