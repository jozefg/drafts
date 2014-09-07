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

Pattern matching in Agda is pretty much identical to Haskell. We can
define something like

``` agda
    not : Bool -> Bool
    not true  = false
    not false = true
```

One big difference between Haskell and Agda is that pattern matching
**must** be exhaustive. Nonexhaustiveness is a compiler error in Agda.

This brings me to another point worth mentioning. Remember that
structural induction I mentioned the other day? Agda only allows
recursion when the terms we recurse on are "smaller".

In other words, all Agda functions are defined by structural
induction. This together with the former restriction means that Agda
programs are "total". In other words all Agda programs reduce to a
single value, they never crash or loop forever.

This can occasionally cause pain though since not all recursive
functions are modelled nicely by structural induction! A classic
example is merge sort. All the examples are carefully written to be
defined in this way, but it will incur certain awkward bits in our
code. Bear this restriction in mind as you continue reading.

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

Agda's GGADTs also allow us to us to add "parameters" instead of
indices. These are things which the data type may use, but each
constructor handles uniformly without inspecting it.

For example a list type depends on the type of it's elements, but it
doesn't poke further at the type or value of those elements. They're
handled "parametrically".

In Agda a list would be defined as

``` agda
    data List (A : Set) : Set where
      nil  : List A
      cons : A -> List A -> List A
```

This is closer to how Haskell's vanilla ADTs work. It has some
important benefits for Agda, but they poke further than I'm willing to
go in such a short post. I'm mentioning them here because our examples
will use both parameters and indices.

Finally, Agda's prelude is absolutely tiny. By tiny I mean essentially
non-existant. Because of this I'm using the Agda standard library
heavily and to import something in Agda we'd write

    import Foo.Bar.Baz

This isn't the same as a Haskell import though. By default, imports in
Agda import a qualified name to use. To get a Haskell style import
we'll use the special shortcut

    open import Foo.Bar

which is short for

    import Foo.Bar
    open Bar

Because Agda's prelude is so tiny we'll have to import things like
booleans, numbers, and unit.

### A Few Examples

Now that we've seen what dependent types look like in Agda, let's go
over a few examples of their use.

First let's import a few things




### Bonus Coolness

### Wrap Up
