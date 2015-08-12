---
title: Notes on Proving Equivalence
tags: types
---

These last few weeks I've been reading a lot about something called
"logical relations" and I thought it might be fun to write about the
general problem logical relations aim to solve (as well as LR
themselves) since it's a pretty huge one in computer science.

Put briefly, logical relations and the other techniques we're going to
discuss are ways of formally proving that two programs are
equivalent. We want an appropriately coarse notion of equivalence. For
example, I want to be able to prove that a program using a list as a
set and a program which uses a splay set will are equivalent because I
won't be to get them to produce different results. Of course, we don't
want *to* coarse an equivalence, otherwise I'll just say "all things
are equivalent" and it's useless.

In this post we go 4 different things

 1. Formally defining how equivalence should work
 2. Go through a techinique for proving equivalence called
   "applicative bisimulation"
 3. Go through logical relations as another method of proving
    observational equivalence

## Observational Equivalence

Before we can get into defining cool tricks for proving equivalence we
have to define what we're going to judge as equivalence. In order to
do this we're going to a "context". A context is just a program with a
whole in it. So suppose we have the simply typed lambda calculus

    e ::= λx. e | e e | x | true | false

Then we could define the set of contexts for it

    C ::= □ | C e | e C | x | true | false

With a context we usually define something like `C{e}` which
represents filling in all the □'s in `C` with `e`.

    □{e}      = e
    (C e'){e} = C{e} e'
    (e' C){e} = e' C{e}
    x{e}      = x
    true{e}   = true
    false{e}  = false


Furthermore, we can define a type relation on contexts just like
terms. The judgment is

    C : Γ ⊢ τ ⇝ Γ' ⊢ τ'

This means that when `C` is filled in with a program `e` so that `Γ ⊢
e : τ` holds, then `Γ' ⊢ C{e'} : τ'` also holds.

Now to define we say that `e : τ` is "observationally equivalent" (or
just equivalent) to `e' : τ` if `C{e} ≡ C{e'}` for all `C : τ ⇝ bool`,
we notate this as `e ≅ e'`. Here I write `≡` to mean literal,
syntactic equality. We know that these contexts run to a boolean value
so `true ≡ true` and `false ≡ false` and nothing else!

A context let's us write any program with a hole in it, `e ≅ e'` that
means that no program can distinguish between the two. This is just
the sort of equality you want for most applications. In compiler
optimizations, if two things are observationally equivalent you're
golden: there's no way you've broken something by switching out a term
for the optimized version because no one can tell! If you're writing
a library, if you swap out things for observationally equivalent
counterparts, you don't have to worry about breaking any programs
because the consumer cannot distinguish between the new and old
versions!

In the rest of this post (and most of literature) we want to come up
with ways to prove theorems along the lines of `e ≅ e'`. However, this
isn't as easy as it sounds! The issue is that by quantifying over *all
possible contexts* we've really made things hard to work with. It's
incredibly easy to use a proof that `e ≅ e'`, but coming up with one
invariably devolves to tricky induction over program contexts! In
order to deal with this, people have come up with a variety of
counterparts which aren't as obviously acceptable as a notion of
equivalence. Instead they're easier to prove and come with a theorem
saying they coincide with `≅`.

## Applicative Bisimulation

The first trick for proving observational equivalence we're going to
look at is called "applicative bisimulation". The basic idea is to say
that `e` is bisimilar to `e'` if they behave the same no matter how we
run them.

We don't quantify over full contexts, instead applicative bisimulation
will say that two terms are equal if

 1. They both loop or run to some values `v1` and `v2`
 2. Then, depending on the type of the expressions, `elimOp(v1)` and
 `elimOp(v2)` must be bisimilar.

Here when I say `elimOp` I'm carelessly notating applying the
elimination forms for a type. Elimination forms here being PL-speak
for what operations are available to *use* an expression of a
particular type. So we can apply functions, we can project out fields
from tuples and similar. To give an example,



## Logical Relations

## Wrap Up
