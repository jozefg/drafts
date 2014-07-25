---
title: All Data Big and Small
---

Programs have all sorts of data floating around. In Haskell programs
in particular, we can make mostly divide the our data into two sorts,
infinite and finite.

Finite data is absolutely everywhere

``` haskell
    foo :: Int
	foo = 1 -- Clearly finite

    bar :: [Bool]
	bar = [True, False] -- Also obviously finite

    baz = bar !! foo
```

In most programming languages, this is it. We have data, and that data
is finite in "size". I put scare-quotes around size here because I'm
using it in a wishy-washy way we'll make formal shortly.

In Haskell and other lazy languages however, we have an alternative,
infinte data!

``` haskell
    fib :: [Integer]
	fib = 0 : 1 : zipWith (+) fib (tail fib)
```

It's clear that if we were to try to determine the length of `fib`,
we'd only succeed in making our machine very hot. There are useful
things we *can* do however, focused on pattern matching on `fib`. We
can use `head`, we can use `tail`. In fact the first thing we might do
is use `take` to determine that we really do have a list of Fibonacci numbers.

So we have operations that work only on finite data, and some that
work on both finite and infinite data. It's worth asking ourselves
what the difference is and how to distinguish between the two.

## All About Recursion

It's worth noticing at this point that the only time we can spot the
difference between functions that are limited to finite data and
infinite data is when they use recursion.

This makes sense if you think about it for a minute. Infinite data in
Haskell has to be built from a recursive type, like lists, trees, etc
etc. Sometimes the types are mutually recursive

``` haskell
    data Big   = Big Int Thing
	data Thing = Thing Big
```

Now the only way to build such types is either using value level
recursion, or writing out the literal terms. Clearly every literal is
finite, so that only leaves recursion.

So the key difference between something like `take` and `length` must
be *how* we recurse. Let's now discuss the ideas behind these two
methods of recursion.

## Induction

The first way of using recursion is the method most programming languages
always support. It lets us deal with 
