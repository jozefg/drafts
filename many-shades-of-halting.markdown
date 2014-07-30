---
title: Many Shades of Halting Oracles
---

I'm going to a take a quick break from arguing with people on the
internet to talk about a common point of confusion with theorem
provers.

People will often state things like "A program in Coq never
diverges" or that "we must prove that X halts". To an outsider, that
sounds impossible! After all, isn't the halting problem undecidable?

Now the thing to realize is that while *yes* the halting problem is
undecidable, we're not solving it. The halting problem essentially
states

> For an arbitrary turing machine P. There is no algorithm guaranteed
> to terminate that will return true if P halts and false if it
> diverges.

In theorem provers, we cleverly avoid this road block with two simple
tricks.

### Being Negative

A program in Coq *must* halt. To do otherwise would introduce a
logical inconsistency. So to enforce this we need to statically decide
whether some program halts.

We just said that this is impossible though! To escape this paradox
Coq opts for a simple idea: reject good programs.

Rather than guaranteeing to return true for every good program, we
state that we'll definitely reject all bad programs and then some.

For example, this termination checker would be logically consistent

``` haskell
    terminates :: CoqProgram -> Bool
    terminates _ = False
```

It'd be useless of course, but consistent. Coq therefore accepts a
certain set of programs which are known to terminate. For example,
ones that limit themselves only to guarded coinduction or structural
induction.

### Getting Our Hands Dirty

While it may be impossible to decide the termination of an arbitrary program,
it's certainly possible to prove the termination of a specific
program.

When Coq's heuristics fail, we can always resort to manually proving
that our code will terminate. This may not be pleasant, but it's
certainly doable. By lifting the burden of Coq, we go from
"constructing arbitrary proof of termination" to "checking arbitrary
proof of termination", which is decidable.

In Coq we can do this with
[well founded recursion](http://adam.chlipala.net/cpdt/html/GeneralRec.html). Simply
put, well founded recursion means that we shift from using only term
"size" to decide what's a smaller recursive call.

To this end, we define a relation for some type `A : Set`,
`R : A -> A -> Prop`. Read `R x y` as `x` is smaller than `y`.

Now we must show that this relation preserves some definition of
"sanity". This should mean that if when a function receives `x`, for
any `y` so that `R y x`, we should be able to recurse on y.

This should also mean that there's no infinite stack of terms so that
`R x y`, `R z x`, `R w z` .... because this would mean we could
recurse infinitely.

To prove this, we must prove `well_founded A R`. What's this "well
founded" thing you say?

Well it's just

    Definition well_founded A R := forall a : A, Acc R a

This `Acc` thing means "accessible",

    Inductive Acc (A : Type) (R : A -> A -> Prop) (x : A) : Prop :=
        Acc_intro : (forall y : A, R y x -> Acc R y) -> Acc R x

So something is accessible in `R` if everything less than it is also
accessible. Notice that this gives us something to induct upon! For
any `Acc` we induct upon it to talk about any element less than `x`
according to `R`.

This is handed to us by the lovely `Fix` (uppercase).

    Fix : well_founded R ->
        forall P : A -> Type,
          (forall x : A, (forall y : A, R y x -> P y) -> P x) ->
          forall x : A, P x

So `Fix` is the better, cooler version of structural recursion that we
were after. It lets us recurse on any `y` where `R y x`.

So in some sense, you can view Coq's Fixpoint as just a specialization
of `Fix` where `R x y` means that `x` is a subterm of `y`.

## Wrap Up

So in conclusion, theorem provers don't do the impossible. Rather they
have a small battery of tricks to cheat the impossible general case
and simplify common cases.

Back to the internet I go.
