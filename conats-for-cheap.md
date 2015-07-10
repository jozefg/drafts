---
title: Coinduction in JonPRL for Low Low Prices
tags: jonprl
---

So as a follow up to my prior [tutorial on JonPRL][tutorial] I wanted
to demonstrate a nice example of JonPRL being used to prove something

 1. Interesting
 2. Unreasonably difficult in Agda or the like

    *I think I'm asking to be shown up when I say stuff like this...*

I would like to implement the conatural numbers in JonPRL but without
a notion of general coinductive *or* even inductive types. Just the
natural numbers. The fun bit is that we're basically going to lift the
definition of a coinductively defined set straight out of set theory
into JonPRL!

## Math Stuff

First, let's go through some math. How can we formalize the notion of
an coinductively defined thing? Well we start with some equation of
the form

    Φ = 1 + Φ

Here I'm using `1` and `+` more in the sense of type and set theory
(discriminated union) than in the sense of "this is a really big
number". This particular equation is actually how we would go about
defining the natural numbers. If I write it in a more Haskellish piece
of notation we'd have

    data Φ = Zero | Succ Φ

Next we transform this into a function. This step is a
deliberate move so we can start applying the myriad of tools we know
of to handle functions to handling this equation.

    Φ(X) = 1 + X

We now what to find some `X` so that `Φ(X) = X`. If we can do this
than I claim that `X` is a solution to the equation given above since

    X = Φ(X)
    X = 1 + X

Precisely mirrors the equation we had above. Such an `X` is called a
"fixed point" of the function `Φ`. However there's a catch, there may
well be more than one fixed point of a function! Which one do we
choose? The key is that we want the coinductively defined
version. In order to find this we want to *largest* fixed point. This
doesn't tell us how to get it, but that's definitely what we want.



## The Code

## The Clincher

## Wrap Up


[tutorial]: /posts/2015-07-06-jonprl.html
