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

At a high level parametricity is about the behavior of well typed
terms with a polymorphic type. Since the type is polymorphic, we know
that it can do only a limited amount of things. For example, the type

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

We can now set out to define
