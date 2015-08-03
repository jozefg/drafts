---
title: Running a JonPRL Extract
tags: compilers, jonprl, sml
---

JonPRL is a proof assistant built on an untyped computation
system. Conceptually, we have

 1. Some system of computation
 2. A type theory built by defining partial equivalence relations on
    the set of terms
 3. A sequent calculus made to bundle the type theory into a form
    suitable for a proof assistant

But underneath everything you see is this pretty reasonable
programming language. In fact, every proof you write in JonPRL gives
rise to some program in this language. At Cornell, they write proofs
in NuPRL and *run* the extracts. This is similar to the process of
taking a Coq proof and getting an ML program but it's not an ad-hoc
erasure process, it's integral to the type theory!

However currently in JonPRL there's no way to really run the
extracts. You can do simple reductions interactively, either using the
new `Eval` command or using the `reduce` tactic while proving a
theorem. On the other hand, I like writing compilers/interpreters and
this is a thinly veiled excuse for me to write one. Here is the rough
sketch for this post

 1. Describe the semantics of JonPRL's computations system
 2. Walk through an awful but simple substituting interpreter
 3. Walk through a cooler interpreter built around nice lazy graph
    reduction

While this post says the word "JonPRL" a lot, most of it is just
interpreter hacking for an untyped language. There's hopefully
something interesting here even if you don't care at all about PRLs.

## The Language

First we have to define our language. Briefly, our language has

 - Lambdas (`lam` and `ap`)
 - Pairs/Products (`pair` and `spread` which is a fusion of `fst` and `snd`)
 - Sums (`inl`, `inr`, and `decide` which is a funnily named `case`)
 - Unit (`ax`)
 - Natural numbers (`zero`, `succ`, and `natrec`)
 - Recursion (`fix`)
 - A strict version of let (`cbv`)

In addition, each type former in JonPRL is a term in our
language. This means that we don't just have `lam` and `ap`, we also
have `fun` (the type of functions) as a term. It behaves just like any
constant, there's no notion of a typing judgment in this language so
there's no special relationship between `lam` and `fun` anymore then
there is one between `inl` and `fun`. The terms I mentioned above are
the only terms which do any computation so I'll just spell out how
*they* work in our interpreter. If you read the actual code you'll see
that I have cases for all the terms we use as types as well, but
they're all boring so who cares.

Additionally, this language is lazy. This means that the evaluation
semantics are pretty straightforward (with the exception of `cbv`). We
just evaluate a term until the top level is not `ap`, `spread`,
`decide`, `natrec`, `fix`, or `cbv` and then we're done. The
evaluation rules for `natrec` for example comes in 3 parts.

                       e ↦ e'
     ——————————————————————————————————————————
     natrec(e; z; x.y.s) ↦ natrec(e'; z; x.y.s)

     ——————————————————————————————————————————
           natrec(zero; z; x.y.s) ↦ z

     ———————————————————————————————————————————————————————
     natrec(succ(n); z; x.y.s) ↦ [n, natrec(n; z; s)/x, y] s


If you prefer to see some code, `natrec` behaves kind of like this
function

``` haskell
    natrec :: Nat -> a -> (Nat -> a -> a)
    natrec Zero z s = z
    natrec (Succ n) z s = s n (natrec n z s)
```

So for example, here's how we'd write factorial in JonPRL,

``` jonprl
    fact = lam(n. natrec(n; succ(zero); n'.rest. n * rest))
```

But now we have to define `*` as it's not primitive

``` jonprl
    * = lam(n. lam(m. natrec(n; m; _.rest.m + rest)))
    + = lam(n. lam(m. natrec(n; m; _.rest.succ(rest))))
```

A few things to note here: first of all binding is uniformly
represented as `var.term`. Lambdas bind one variable so instead of `fn
x => ...` or `\x -> ...` we say `lam(x. ...)`. Similarly, the 3rd part
of `natrec` binds two arguments so we have `x.y.`. Second of all,
notice that `natrec` plays both the role of pattern matching and
providing recursion. While we have a special `fix` combinator it's
rare we need to use it since we can get away with `natrec` and friends
most of the time.

As another example, we could

## The Reference Interpreter

## The Graph Reduction Interpreter

## Wrap Up
