---
title: Some Thoughts on Parametricity
tags: types
----

I like types. If you haven't figured this out from my blog I really
don't know where you've been looking :) If you've ever talked to me in
real life about why I like types, chances are I mentioned ease of
reasoning and correctness.

In this post I'd like to talk about one of the more powerful tools we
have for reasoning about typed programs: parametricity. Parametricity
is the workhorse behind Wadler's free theorems, why PHOAS always
generates closed terms, and a whole bunch of other cool stuff.

Actually proving that parametricity is quite subtle. To do it from the
ground up we'd have to formalize a lot of niggly details about
static and dynamic semantics and then lots of induction. Instead I'd
like to show how to rigorously apply parametricity. So we'll be a step
above handwaving and a step below actually proving everything correct.

## What is Parametricity

At a high level parametricity is about the behavior of well typed
terms. Since the type is polymorphic, we know that it can do only a
limited amount of things. For example, the type

``` haskell
    const :: a -> b -> a
```

Tells us everything we need to know about `const`. It returns it's
first argument. In fact, if it returns anything (non-bottom) at all,
it simply *must* be its first argument!

Parametricity isn't limited to simple cases like this however, it can
be used to prove that the type

``` haskell
    forall c. c -> (a -> c -> c) -> c
```

Is completely isomorphic to `[a]`!

We can use parametricity to prove free theorems, like

if

``` haskell
    map id = id
```

then

``` haskell
    map f . map g = map (f . g)
```

These are non-obvious properties and yet parametricity gives us the
power to prove all of them without even looking at the implementation
of these functions. That's pretty cool!

## Handwavy Parametricity

In order to get an idea of how to use parametricity, let's do some
handwavy proofs to get some intuition for how parametricity works.

Start with `id`.

``` haskell
    id :: a -> a
```

We know right away that `id` takes some value of type `a` and returns
another value `a`. Most people would safely guess that the returned
value is the one we fed it.

In fact, we can kinda see that this is the *only* thing it could
do. If it didn't, then somehow it'd have to create a value of type
`a`, but we know that that's impossible!

Similarly, if `map id` is just `id`, then we know that `map` isn't
randomly dropping some elements of our list. Since `map` isn't
removing elements, in order to take an `a` to a `b`, `map` has to be
applying `f` to each element! Since that's true, we can clearly see
that

``` haskell
    map f . map g = map (f . g)
```

because we know that applying `f` and then applying `g` is the same as
apply `f` and `g` at the same time!

Now these handwavy statements are all based on one critical point. No
matter how we instantiate a type variable, the behaviour we get is
related. Instantiating something to `Bool` or `Int` doesn't change the
fundamental behaviour about what we're instantiated.

We can now set out to define rigorously define what we're really
talking about.

## Background

Before we can formally define parametricity we need to flesh out a few
things. First things first, we need to actually specify the language
we're working in. For our purposes, we'll just deal with pure System
F.

    ty ::= v                [Type Variables]
           ty -> ty         [Function Types]
           forall v. ty     [Universal Quantification]
           Bool             [Booleans]

    exp ::= v               [Variables]
            exp exp         [Application]
            \v : ty -> exp  [Abstraction]
            Λv -> exp      [Type Abstraction]
            exp[ty]         [Type Application]
            true            [Boolean]
            false           [Boolean]

The only real notable feature of our language is that all polymorphism
is explicit. In order to have a full polymorphic type we have to use a
"big lambda" Λ. This acts just like a normal lambda except instead of
abstracting over a term this abstracts over a type.

For example the full term for the identity function is

    id = Λ A -> \x : A -> x

From here we can explicitly specialize a polymorphic type with type
application.

    id[Bool] true

Aside from this, the typing rules for this language are pretty much
identical to Haskell's. In the interest of brevity I'll elide them.

Now that we have a reasonable framework to go from, we can discuss a
more precise formulation of parametricity.

## Actual Parametricity

Now that we have our language, let's talk about what we're interested
in proving. Our basic goal is to show that two expressions `e1` and
`e2` are equal. However, we don't want to use a `==` sort of
equality. We really mean that they can't be distinguished by our
programs. That for all programs with a "hole", filling that hole with
`e1` or `e2` will produce identical results. This is called
"observational equivalence" usually and notated with `≅`.

This is a bit more general than just `==`, for example it let's us say
that `flip const () ≅ id`. Now let's define another notion of
equality, logical equivalence.

This logical equivalence is an attempt to define equality without just
saying "running everything produces the same result". It turns out
it's really really hard to prove things that aren't syntactically
equivalent will always produce the same result!

Our logical equivalence `~` is defined in a context `η : δ ↔ δ'`. The
reason for this is that our terms may have free type variables and we
need to know how to deal with them. Each δ maps the free types in the
types of our terms to a concrete types and η is a relationship for
comparing `δ(v)` with `δ'(v)`.

Put less scarily, `η` is a set of rules that say how to compare two
terms when the have both are of type `v`. This is an important part of
our logical relation: it deals with open terms, terms with free
variables.

Now η isn't composed of just any relationship between terms, it has to
be "admissible". Admissibility means that for some relation R, two
conditions hold

 1. If `e R e'` and `d ⇒ e` and `d' ⇒ e'`, then `d R d'`
 2. If `e R e'` and `d ≅ e` and `d' ≅ e'`, then `d R d'`

The first rule means that `R` is closed under evaluation and the
second says that `R` respects observational equivalence.

Now we define our logical equivalence in some context δ to be

 1. When `e, e' : τ`, `e ~ e' [η]`
    if `e δ(t) e'`
 2. When `e, e' : Bool`, `e ~₂ e' [η]`
    if `e ⇓ v` and `e' ⇓ v`
 3. When `f, g : a → b`, `f ~ g [η]`
    if for all `a b : a`, when `a ~ b [η]`, `f a ~ g b [η]`
 4. When `e e' : forall v. t`, `e ~ e' [η]`
    if for all `R : p ↔ p'`, `e[p] ~ e'[p'] \[η[v ↦ R]\]`

Now this rule has 4 cases, one for each type. That's the first
critical bit of this relation, we're talking about things by the
structure of the type, not the value itself.

Now with this in mind we can state the full parametricity theorem.

> For all expressions e and δ, `e ~ e [̣̣δ]`

That's it! Now this is only really useful when we're talking about
polymorphic type, then parametricity states that for any admissible
relation `R`, two different instantiations are related.

While I won't go into how to prove it, another important results we'll
use for proofs with parametricity is that `(∀η. e ~ e' [η]) ⇔ e ≅ e'`.

## Applying Parametricity

Now that I've said exactly what parametricity is, I'd like to step
through a few proofs. The goal here is to illustrate how we can use
this to prove some interesting properties.

First we just have to prove the classic result that any
`f : forall a. a -> a` is equivalent to `id = Λa. λx : a. x`.

To prove this we need to show `f ~ id [δ]`. For this we need to show
that for any admissible relation `R` between `τ` and `τ'`, then
`f[τ] ~ λx : τ'. x [δ + R]`. Stepping this one more time we end up
with the goal that `e R e'` then `f[τ] e ~ e' ⇔ f[τ] e R e'`

Now this is where things get tricky and where we can apply
parametricity. We know by definition that `f ~ f [δ]`. We then choose
a new relation `S : τ' ↔ τ'` where `d S d'` if and only `d ≅ e'` and
`d' ≅ e'`. Exercise to the reader: show admissibility.

From here we know that `f[τ] ~ f[τ] \[η[a ↦ R]\]` and since `e S e`
then `f[τ] e ~ f[τ] e` which implies `f[τ] e S f[τ] e`. This means
that `f[τ] e ≅ e`.  From our note above, `f[τ] e ~ e` and by
transitivity we have `f[τ] e R e'`.

Now notice how we applied parametricity here, we took advantage of the
fact that `~` is reflexive across all relationships to force `f[τ] e`
into something useful. This is the general idea of most parametricity
proofs.

Now for something a bit more challenging,

## A Note on Free Theorems

## Wrap Up
