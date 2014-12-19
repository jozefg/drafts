---
title: What Is The Monomorphism Restriction
tags: haskell
---

One of the more puzzling features of Haskell for new comers is the
monomorphism restriction. I think that part of the reason behind this
that

 1. We call it the "dreaded" MR, it's not nearly that bad
 2. We very rarely explain exactly what it is and why it exists

I've written about this before, but it was in on a blog long ago with
a url far far away.

## What is the Monomorphism Restriction?

The monomorphism restriction is an artifact of how Haskell is type
checked. In Haskell programs we have a few ways to introduce
variables, like

``` haskell
    let x = ... in ...
```

Further more, we expect `x` to be given the most general type it can
be given. This is called "let generalization" but it also applies to
`where` clauses and top level values. Not everything in Haskell is
generalized, specifically lambdas embedded in expressions are not
always generalized. There's a good reason for this (things get
undecidable quickly) but we need not worry about this for the moment.

The result of this is that we can do something like

``` haskell
    let f = \x -> x in f "foo" ++ show (f 'c')
```

Notice how we applied `f` to both a `String` and `Char` so `f` must
have the proper polymorphic type

``` haskell
    f :: a -> a
```

The monomorphism restriction is just the following: If `f` is a value
bound in a `let` expression or similar which has

 1. No arguments, in constant applicative form (like our `f`)
 2. A type involving a type class constraint
 3. Has no signature

`f` will not be generalized, but instead instantiated with special
"monomorphic" type variables. These monomorphic type variable
represent one specific type but we're not sure which type yet. This
means that when we actually use the value they'll be filled in with
real concrete types retroactively.

To illustrate this, consider

``` haskell
    let f = show in f "foo" ++ f 'c'
```

Here `f` is in constant applicative form and its most general type is

``` haskell
    forall a. Show a => a -> String
```

So rather then give it this type, we instead give it

``` haskell
    for some a. Show a => a -> String
```

That `for some` is pseudo-notation since there isn't a way to utter
monomorphic type variable. We then apply `f` to `"foo"` so we
retroactively fill in `a` so `f :: String -> String`. However, we then
go and try to apply it to `'c' :: Char`, since `Char /= String` we're
get a type error!

Confusingly all this type error says is that

    Couldn't match expected type ‘[Char]’ with actual type ‘Char’
    In the first argument of ‘f’, namely ‘'c'’
    In the second argument of ‘(++)’, namely ‘f 'c'’
    In the expression: f "foo" ++ f 'c'

and makes no mention of the monomorphic type variables at play
here. In general, when you see these weird type errors complaining
that two obviously different types aren't equal, keep an eye out for
the monomorphism restriction.

## Why Does The Monomorphism Restriction Exist?

Now it's not terrible to see where the monomorphism restriction
applies, but why on Earth does it exist? The rational behind it is
actually to do with how type classes are implemented.

In GHC (and all other Haskell compilers I'm aware of) the constraint
`C a => ...` is actually transformed into a proper function and type
classes instances become records of functions. So `f`'s type might
expand into

``` haskell
    data Show a = Show {show :: a -> String}
    f :: Show a -> a -> String
```

What's interesting about this is *Haskell functions aren't shared*. By
this I mean that given

``` haskell
    first :: a -> b
    first = let veryExpensiveValue = veryExpensiveComputation
            in \a -> ...

    second :: a -> b
    second a = let veryExpensiveValue = veryExpensiveComputation
               in ...
```

In `first` we'll compute `veryExpensiveValue` once while in second we
compute it on every application. This can have very
[real impacts on performance][so-question]. In fact, I often write
code that depends on sharing and what not to increase the asymptotic
performance of my code. Take for example this function to compute the
powers of 2.

``` haskell
    pow2 :: forall a. (Eq a, Num a) => a -> a
    pow2 0 = 1
    pow2 n = n' + n'
      where n' = pow2 (n - 1)
```

By only computing `pow2`'s recurrence once this runs in linear
time rather than exponential.

[so-equestion]: http://stackoverflow.com/a/19209349/784338
