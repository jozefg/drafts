---
title: Introduction to Dependent Types: Haskell on Steroids
---

I'd like to start another series of blog posts. This time on something
that I've wanted to write about for a while, dependent types.

There's a noticeable lack of accessible materials introducing
dependent types at a high level aimed at functional
programmers. That's what this series sets out help fill. Therefore, if
you're a Haskell programmer and don't understand something, it's a
bug! Please comment so I can help make this a more useful resource for
you :)

Alright, to kick this off we're going to start writing some Haskell
code. Really we're going to try pushing some GHC extensions to their
limits.

## Kicking GHC in the Teeth

Let's start with the flurry of extensions we need

    TODO, FILL THIS OUT

Now our first definition is a standard formulation of natural numbers

``` haskell
    data Nat = Z | S Nat
```

Here `Z` represents 0 and `S` means `+ 1`. So you should read `S Z` as
1, `S (S Z)` as 2 and so on and so on.

We can use this definition to formulate addition

``` haskell
    plus :: Nat -> Nat -> Nat
    plus Z n     = n
    plus (S n) m = S (plus n m)
```

This definition proceeds by "structural induction". That's a scary
word that pops up around dependent types. It's not all that
complicated, all that it means is that we use recursion only on
*strictly smaller terms*.

There is a way to formally define smaller, if a term is a constructor
applied to several (recursive) arguments. Any argument to the
constructor is strictly smaller than the original terms. In a strict
language if we restrict ourselves to only structural recursion we're
guaranteed that our function will terminate. This isn't quite the
case in Haskell since we have infinite structures.

``` haskell
    toInt :: Nat -> Int
    toInt (S n) = 1 + toInt n
    toInt Z     = 0

    bigNumber = S bigNumber

    main = print (toInt bigNumber) -- Uh oh!
```

Often people will cheerfully ignore this part of Haskell when talking
about reasoning with Haskell and I'll stick to that tradition (for now).

Now back to the matter at hand. Since our definition of `Nat` is quite
straightforward, it get's promoted to the [kind][kind-exp] level by
`DataKinds`.

Now we can "reflect" values back up to this new kind with a second
GADTed definition of natural numbers.

``` haskell
    data RNat :: Nat -> * where
      RZ :: RNat Z
      RS :: RNat n -> RNat (S n)
```

Awesome! What on earth could that be useful for? Well with this we can
do something fancy with the definition of addition.


``` haskell
    type family Plus n m :: Nat where
      Plus Z n     = n
      Plus (S n) m = S (Plus n m)
```

Now we've reflected our definition of addition to the type
family. More than that, what we've written above is fairly obviously
correct. We can now force our value level definition of addition to
respect this type family

``` haskell
    plus' :: RNat n -> RNat m -> RNat (Plus n m)
    plus' RZ n     = n
    plus' (RS n) m = RS (plus' n m)
```

Now if we messed up this definition we'd get a type error!

``` haskell
    plus' :: RNat n -> RNat m -> RNat (Plus n m)
    plus' RZ n     = n
    plus' (RS n) m = plus' n m -- Unification error! n ~ S n
```

Super! But this still hasn't bought us that much, we've just shifted
the bugs to a new higher level! What if our type family is wrong?

We could write a few tests, but that's not satisfying enough! We could
write tests for functions after all. If we've gone through all this
trouble we really ought to get some stronger guarantees.

A simple guarantee should be that `Plus Z n` should always be
`n`. This is to say that zero is the "left identity" for `Plus`. Now,
how could we go about showing that? We could opt to do something like

    data Dict c = c => Dict c

And use the `~` equality constraint. That's an unwieldy approach
however, since we have now way of manipulating that equality. Instead
we can opt for another GADT.

    data Eq :: k -> k -> * where
      Refl :: Eq a a

This definition says that `Eq` is a map from some kind `k` to `k` to
`*`. The only way we can create an `Eq a b` is if `a` unifies with `b`
so therefore, if `Eq a b` than `a` is the same thing as `b`.

Shweet, now we can at least state something like the left identity
property we wanted.

``` haskell
    leftId :: Eq n (Plus Z n)
    leftId = _
```

Now if we can fill in that `_`, then we know that this holds for all
`n` because GHC type checker says so. What on Earth could we fill it
with though.. let's try `Refl`!

``` haskell
    leftId :: Eq n (Plus Z n)
    leftId = Refl
```

And it goes through. This proof goes through so easily since
`Plus Z n` is simply defined to be `n`. This is hardly a fair
example. Let's try proving that zero is also the right identity.

``` haskell
    rightId :: Eq n (Plus n Z)
    rightId = Refl
```

Urk type errors! It's clear why though, GHC isn't smart enough to try
to prove this on it's own and it doesn't follow from straightforward
reduction. So what can we do? Let's try that structural induction
thing. To give us something to induct upon we'll pass in that `n` with
an `RNat n`, a concrete value term.

``` haskell
    rightId :: RNat n -> Eq n (Plus n Z)
    rightId RZ = Refl -- Base case
    rightId (RS n) = case rightId n of
      Refl -> Refl
```

There are 3 interesting bits to this function. First is the base case,
when we pattern match with `RZ`, `n` is forced to be `Z` we can then
just ask GHC to prove everything by reduction. For `RS n` we need to
pattern match on `rightId`. This forces the equality `Plus n Z ~ n`
into scope, from there GHC can figure out the proof for us.

How far can we take this though? We can go on to prove things like
commutativity and associativity, but it's painful! All this hammers on
the fact that Haskell was not intended to support this form of
proofs/contract style programming.

In fact it'd be nearly impossible to replace the definition of `RNat`
or `plus'` with a more efficient representation without modifying
their type level equivalents so we're not gaining much here. The whole
point after all is to shift trust to GHC, but all we're really doing
is duplicating code.

## Fitting GHC with Steel Dentures



[kind-exp]: TODO FILL IN LINK
