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
an coinductively defined type as we're used to in programming
languages? Recall that something is coinductively if it contains all
terms so that we can eliminate the term according to the elimination
form for our type. For example, Martin-Lof has proposed we view
functions (Π-types) as coinductively defined. That is,

    x : A ⊢ f(x) : B(x)
    ————————————————————
     f : Π x : A. B(x)

In particular, there's no assertion that `f` needs to be a lambda,
just that `f(x)` is defined and belongs to the right type. This view
of "if we can use it, it's in the type" applies to more than just
functions. Let's suppose we have a type with the following elimination
form

    L : List  M : A  x : Nat, y : List : A
    ——————————————————————————————————————
          case(L; M; x.y.N) : A

This is more familiar to Haskellers as

    case L of
      [] -> M
      x :: y -> N

Now if we look at the coinductively defined type built from this
elimination rule we have not finite lists, but streams! There's
nothing in this elimination rule that specifies that the list be
finite in length for it to terminate. All we need to be able to do is
evaluate the term to either a `::` of a `Nat` and a `List` or
`nil`. This means that

    fix x. cons(0; x) : List

Let's now try to formalize this by describing what it means to build a
coinductively type up as a set of terms. In particular the types we're
interested in here are algebraic ones, constructed from sums and
products.


Now unfortunately I'm going to be a little handwavy. I'm going to
act is if we've worked out a careful set theoretic semantics for this
programming language (like the one that exists for MLTT). This means
that All the equations you see here are across sets and that these
sets contain programs so that `⊢ e : τ` means that `e ∈ τ` where `τ`
on the right is a set.

Well we start with some equation of the form

    Φ = 1 + Φ

This particular equation a is actually how we would go about
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
version. Coinduction means that we should always be able to examine a
term in our type and its outermost form should be `1 + ???`. Okay,
let's optimistically start by saying that `X` is `⊤` (the collection
of all terms).

Ah okay, this isn't right. This works only so long as we don't make
any observations about a term we claim is in this type. The minute we
patter match, we might have found we claimed a function was in our
type! I have not yet managed to pay my rent by saying "OK, here's the
check.. but don't try to *use* it and it's rent" so perhaps we should
try something else. Okay, so let's not say `⊤`, let's say

    X = ⊤ ⋂ Φ(⊤)

Now since if `t ∈ X`, we know that `t ∈ 1 + ???`. This means that if
we run `e ∈ X`, we'll get the correct outermost form. However, this
code is still potentially broken

``` haskell
    case e of
      Inl _ -> ...
      Inr x -> case e of
                 Inl _ -> ...
                 Inr _ -> ...
```

This starts off as being well typed, but as we evaluate it may
actually become ill typed. If we claimed that this was a fixed point
to our language our language would be type-unsafe. This is an
unappealing quality in a type theory.

Okay, so that didn't work. What if we fixed this code by doing

    X = ⊤ ⋂ Φ(⊤) ⋂ Φ(Φ(⊤))

Now this fixes the above code, but can you imagine a snippet of code
where this still gets stuck? So each time we intersect `X` with `Φ(X)`
we get a new type which behaves like the real fixed point so long as
we only observe `n + 1` times where `X` behaves like the fixed point
for `n` observations. Well, we can only make finitely many
observations so let's just iterate such an intersection

    X = ⋂ₙ Φⁿ(⊤)

So if `e ∈ X` then no matter how many times we pattern match and
examine the recursive component of `e` we know that it's still in
`⋂ₙ Φⁿ(⊤)` and therefore still in `X`! In fact, it's easy to prove
that this is the case with two lemmas

 1. If `X ⊆ Y` then `Φ(X) ⊆ Φ(Y)`
 2. If I have a collection `S` of sets, then `⋂ Φ(S) = Φ(⋂ S)` where
    we define `Φ` on a collection of sets by applying `Φ` to each
    component.

These two properties state the monotonicity and cocontinuity of
`Φ`. We then may show that

    Φ(⋂ₙ Φⁿ(⊤)) = ⋂ₙ Φⁿ(⊤)

As desired.

## The Code

## The Clincher

## Wrap Up


[tutorial]: /posts/2015-07-06-jonprl.html
