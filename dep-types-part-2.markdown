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

### Dependent Types

There wouldn't be much point in writing Agda if it didn't have
dependent types. In fact the two mechanisms that comprise our
dependent types translate wonderfully into Agda.

First we had pi types, remember those?

``` haskell
    foo :: (a :: A) -> B
    foo a = ...
```

Those translate almost precisely into Agda, where we'd write

``` agda
    foo : (a : A) -> B
```

The only difference is the colons! In fact, Agda's pi types are far
more general than what we'd discussed previously. The extra generality
comes from what we allow `A` to be. In our previous post, `A` was
always some normal type with the kind `*` (`Set` in Agda). In Agda
though, we allow `A` to be `Set` itself. In Haskell syntax that would
be something like

``` haskell
    foo :: (a :: *) -> B
```

What could `a` be then? Well anything with the kind `*` is a type,
like `Bool`, `()`, or `Nat`. So that `a` is like a normal type
variable in Haskell

``` haskell
    foo :: forall a. B
```

In fact, when we generalize pi types like this, they generalize
parametric polymorphism. This is kind of like how we use "big lambdas"
in System F to write out polymorphism explicitly.

Here's a definition for the identity function in Agda.

``` agda
    id : (A : Set) -> A -> A
    id _ a = a
```

This is how we actually do all parametric polymorphism in Agda, as a
specific use of pi types.

Now our other dependently typed mechanism was our generalized
generalized algebraic data types. These also translate nicely to Agda.

``` agda
    data Foo : Bool -> Set where
      Bar : Foo True
```

We indicate that we're going to index our data on something the same
way we would in Haskell++, by adding it to the type signature on the
top of the data declaration.

Agda's GGADTs also allow us to

It's almost like someone described Haskell++ with Agda in mind!

### A Few Examples

### Bonus Coolness

### Wrap Up
