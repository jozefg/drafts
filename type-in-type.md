---
title: Type is not in Type
tags: jonprl, types, haskell
---

I was reading a [recent proposal][dep-haskell] to merge types and
kinds in Haskell to start the transition to dependently typed
Haskell. One thing that caught my eye as I was reading it was that
this proposal adds `* :: *` to the type system. This is of some
significance because it means that once this is fully realized Haskell
will be inconsistent. Of course, this isn't a huge deal since Haskell
is already woefully inconsistent with

 - `unsafePerformIO`
 - Recursive bindings
 - Recursive types
 - Exceptions
 - ...

So it's not like we'll be entering new territory here. This is an
issue of significance though for languages like Idris or Agda where
such a thing would actually render proofs useless. Famously,
Martin-Lof's original type theory (the 1971 version AFAIK) did have
`Type : Type` (or `* :: *` in Haskell spelling) and Girard managed to
derive a contradiction (Girard's paradox). The particulars of this
construction are a little bit complicated but it's interesting to note
why exactly this goes wrong though. In this post I'd like to prove
that `Type : Type` is a contradiction in [JonPRL][jonprl]. This is a
little interesting because in most proof assistants this would work in
two steps

 1. Hack the compiler to add the rule
 2. Construct a contradiction and check it with the modified compiler

*OK to be fair, in something like Agda you could use the compiler
 hacking they've already done and just say `{-# OPTIONS --set-in-set
 #-}` or whatever the flag is. The spirit of the development is the
 same though*

in JonPRL I'm just going to prove this as a regular implication. We
have a proposition which internalizes membership and I'll demonstrate
`member(U{i}; U{i}) -> void` is provable.

## Background on JonPRL

Before we can really get to the proof we want to talk about we should
go through some of the more advanced features of JonPRL we need to
use.


**TODO, does this paragraph sound awful? I think maybe.** First,
notice that JonPRL is built a bit differently than other proof
assistants. In JonPRL we *start* with computation and add typing as a
layer on top of a computation system. Programs existed before types so
to speak. In other languages, we have a type system and we use the
local reductions admissible in it to build computation. In JonPRL
though, we can embrace this computational nature and define a type
which contains all closed terms in our language and whose equality is
purely computational. This type is `base`. To prove that `=(a; b;
base)` holds you have to prove `ceq(a; b)` the finest grain equality
in JonPRL. Two terms are `ceq` if they

 1. Both diverge
 2. Run to the same outermost operator and have `ceq` components

What's particularly exciting is that you can substitute any term for
any other term `ceq` to it, no matter at what type it's being used. In
fact, the `reduce` tactic (which beta reduces terms) can conceptually
be thought of as substituting a bunch of terms for their whn forms
which are `ceq` to the original terms. The relevant literature behind
this is found in Doug Howe's "Equality in a Lazy Computation
System". There's more in JonPRL in this regard, we also have the
asymmetric version of `ceq` (called `approx`) but we won't need it
today.

Next, let's talk about the image type. This is a type constructor
with the following formation rule

     H ⊢ A : U{i}        H ⊢ f : Base
     —————————————————————————————————
          H ⊢ image(H; f) : U{i}

So here `A` is a type and `f` is an untyped term in the computation
system JonPRL is built on. Things are going to be equal `image` if we can
prove that they're of the form `f w` and `f w'` where `w = w' ∈ A`. So
`image` gives us the codomain (range) of a function. What's pretty
crazy about this is that it's not just the range of some function
`A → B`, we don't really need a whole new type for that. It's the range of
*literally any closed term we can apply*. We can take the range of the
y combinator over pi types. We can take the range of `lam(x. ⊥)` over
`unit`, anything we want!

This construct let's us define some really incredible things as a user
of JonPRL. For example, the "squash" of a type is supposed to be a
type which is occupied by `<>` (and only `<>`) if and only if there
was an occupant of the original type. You can define these in HoTT
with higher inductive types. Or, you can define these in this type
theory as

``` jonprl
    Operator squash : (0).
    [squash(A)] =def= [image(A; lam(x. <>))]
```

Something is in `x ∈ squash(A)` if and only if we can construct an `a` so
that `a ∈ A` and `lam(x. <>) a ~ x`. Clearly `x` must be `<>` and we
can construct such an `a` if and only if `A` is nonempty.

We can also define the set-union of two types. Something is supposed
to be in the set union if and only if it's in one or the other. Two
define such a thing with an image type we have

``` jonprl
    Operator union : (0).
    [union(A; B)] =def= [image((x : unit + unit) * decide(x; _.A; _.B); lam(x.snd(x)))]
```

This one is a bit more complicated. The domain of things we're
applying our function to this time is

``` jonprl
    (x : unit + unit) * decide(x; _.A; _.B)
```

This is a dependent pair, it's a boolean (true being `inl(<>)` and
false being `inr(<>)`). If the first component is true the second
component is of type `A`, otherwise it's of type `B`. So for every
term of type `A` or `B`, there's a term of this type. In fact, we can
recover that original term of type `A` or `B` by just grabbing the
second component of the term! We don't have to worry about the type of
such an operation because we're not creating something with a function
type, just something in `base`.

`union`s let us define an absolutely critical admissible rule in our
system. See JonPRL has this propositional reflection of the equality
judgement, but it's non-negatable. In order to prove that `=(a; b; A)`
is a proposition we have to make `a = b ∈ A` is evident. So `=(a; b;
A)` is a proposition only if it's true. However, we can add a rule
that says that `=(a; b; A)` is a proposition if `a = b ∈ A ∪ base`!
This rule let's us meaningfully write things like `=(a; b; A) →
void`. Before having a function take a `=(...)` was useless! The
function is only even type-able if its domain was provably true so
there was no point in taking it as an argument. With this rule we can
prove that `=(<>; <>; nat)` is a proposition! Not a true one, but a
well formed one. We can apply this special rule in JonPRL with
`eq-eq-base` instead of just `eq-cd` like the rest of our equality
rules.

## The Main Result

## Wrap Up
