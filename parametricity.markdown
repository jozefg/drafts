---
title: Some Thoughts on Parametricity
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

## Using Parametricity

To begin with, we'll specify what exactly is the language we're
working with.

    ty ::= v                [Type Variables]
           ty -> ty         [Function Types]
           forall v. ty     [Universal Quantification]
           Bool             [Booleans]

    exp ::= v               [Variables]
            exp exp         [Application]
            \v : ty -> exp  [Abstraction]
            /\v -> exp      [Type Abstraction]
            exp[ty]         [Type Application]
            true            [Boolean]
            false           [Boolean]

This language is commonly called "System F". The only real notable
feature of our language is that all polymorphism is explicit. In order
to have a full polymorphic type we have to use a "big lambda" /\. This
acts just like a normal lambda except instead of abstracting over a
term this abstracts over a type.

For example the full term for the identity function is

    id = /\ A -> \x : A -> x

From here we can explicitly specialize a polymorphic type with type
application.

    id[Bool] true

Now that we have our language, let's talk about what we're interested
in proving. Our basic goal is to show that two expressions `e1` and
`e2` are equal, `e1 ≅ e2`. However, we don't want to use a `==` sort
of equality. We really mean that they can't be distinguished by our
programs. That for all programs with a "hole", filling that whole with
`e1` or `e2` will produce identical results.

This is a bit more general than just `==`, for example it let's us say
that `flip const () ≅ id`. Now let's define another notion of
equality, logical equivalence.

This logical equivalence is an attempt to define equality without just
saying "running everything produces the same result". It turns out
it's really really hard to prove things that aren't syntactically
equivalent will always produce the same result!

Our logical equivalence `~` is defined in a context `δ`. The reason
for this is that our terms may have free type variables and we need to
know how to deal with them. δ is just a map from a type variable to a
relation between terms. Now this can't be just any relationship, it
has to be "admissible". Admissibility means that for some relation R,
two conditions hold

 1. If `e R e'` and `d ⇒ e` and `d' ⇒ e'`, then `d R d'`
 2. If `e R e'` and `d ≅ e` and `d' ≅ e'`, then `d R d'`

Notice that R can be between two types or the same types.

Now we define our logical equivalence in some context δ to be


 1. When `e, e' : τ`, `e ~ e' [δ]`
    if `e δ(t) e'`
 2. When `e, e' : Bool`, `e ~₂ e' [δ]`
    if `e ⇓ v` and `e' ⇓ v`
 3. When `f, g : a → b`, `f ~ g [δ]`
    if for all `a b : a`, when `a ~ b [δ]`, `f a ~ g b [δ]`
 4. When `e e' : forall v. t`, `e ~ e' [δ]`
    if for all R between any p and p', `e [p] ~ e'[p']δ + R]`

Now this rule has 4 cases, one for each type. That's the first
critical bit of this relation, we're talking about things by the
structure of the type, not the value itself.

Now with this in mind we can state the full parametricity theorem.

> For all expressions e and δ, `e ~ e [̣̣δ]`

That's it! Now this is only really useful when we're talking about
polymorphic type, then parametricity states that for any admissible
relation `R`, two different instantiations are related.

## Applying Parametricity
