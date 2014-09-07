---
title: Introduction to Dependent Types: Off, Off to Agda Land
---

In this second post of my "intro to dependent types" series we're
going on a whirlwind tour of Agda. Specifically we're going to look at
translating our faux-Haskell from the last post into honest to
goodness typecheckable Agda.

There are 2 main reasons to go through the extra work of using a real
language rather than pseudo-code

 1. This is typecheckable.
    I can make sure that all the i's are dotted and t's crossed.
 2. It's a lot cleaner
    We're only using the core of Agda so it's more or less a very
    stripped down Haskell with a much more expressive but simpler type
    system.

With that in mind let's dive in!

### What's the Same

There's quite a bit of shared syntax between Agda and Haskell, so a
Haskeller can usually guess what's going on.

In Agda we still give definitions in much the same way (single `:`
though)

``` agda
    thingy : Bool
    thingy = true
```

Much like Haskell, we use

``` haskell
    name :: Type
    name = val
```

In fact, we even get Haskell's nice syntactic sugar for functions.

``` agda
    function : A -> B -> ... -> C
    function a b ... = c
```
Will desugar to a lambda.

``` agda
    function : A -> B -> ... -> C
    function = \a b ... -> c
```

One big difference between Haskell and Agda is that, due to Agda's
more expressive type system, type inference is woefully
undecidable. Those top level signatures are not optional sadly. Some
DT language work a little harder than Agda when it comes to inference,
but for a beginner this is a bit of a feature: you learn what the
actual somewhat scary types are.

Like Haskell function application is whitespace and functions are
curried

``` agda

    foo : A -> B -> C
    foo = ...

    a : A
    a = ...

    bar : B -> C
    bar = foo a
```

Even the data type declarations should look familiar, they're just
like GADTs syntactically.

``` agda
    data Bool : Set where
      true  : Bool
      false : Bool
```

Notice that we have this new `Set` thing lurking in our code. `Set` is
just the kind of normal types, like `*` in Haskell. In Agda there's
actually an infinite tower of these `Bool : Set : Set1 : Set2 ...`,
but won't concern ourselves with anything beyond `Set`. It's also
worth noting that Agda doesn't require any particular casing for
constructors, traditionally their lower case.

### What's Different

Obviously I wouldn't be writing this post if Agda was identical to
Haskell! Let's no go over a few places where the two differ.

The most obvious is in the case of polymorphism.

In Haskell, we just write a lowercase word and
