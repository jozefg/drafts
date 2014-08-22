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

There are four parts to this series, each answering one question

 1. What are dependent types?
 2. What does a dependently typed language look like?
 3. What does it feel like to write programs with dependent types?
 4. What does it mean to "prove" something

So first things first, what are dependent types? Most people by now
have heard the unhelpful quick answer

> A dependent type is a type that depends on a value, not just other
> types.

But that's not helpful! What does this actually look like? To try to
understand this we're going to write some Haskell code that pushes us
as close as we can get to dependent types in Haskell.

## Kicking GHC in the Teeth

Let's start with the flurry of extensions we need

    {-# LANGUAGE DataKinds            #-}
    {-# LANGUAGE KindSignatures       #-}
    {-# LANGUAGE GADTs                #-}
    {-# LANGUAGE TypeFamilies         #-}
    {-# LANGUAGE UndecidableInstances #-}

Now our first definition is a standard formulation of natural numbers

``` haskell
    data Nat = Z | S Nat
```

Here `Z` represents 0 and `S` means `+ 1`. So you should read `S Z` as
1, `S (S Z)` as 2 and so on and so on.

If you're having some trouble, this function to convert an `Int` to a
`Nat` might help

``` haskell
    -- Naively assume n >= 0
    toNat :: Int -> Nat
    toNat 0 = Z
    toNat n = S (toNat $ n - 1)
```

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

Super! We know have types that express strict guarantees about our
program. But how useable is this?

To put it to the test, let's try to write some code that reads to
integers for standard input and prints their sum.

We can easily do this with our normal `plus`

``` haskell
    readNat :: IO Nat
    readNat = toNat <$> readLn

    main :: IO ()
    main = plus <$> readNat <*> readNat
```

Easy as pie! But what about `RNat`, how can we convert a `Nat` to an
`RNat`? Well we could try something with type classes I guess

    class Reify a where
      type N
      reify :: a -> RNat N


But wait, that doesn't work since we can only have once instance for
all `Nat`s. What if we did the opposite

    class Reify (n :: Nat) where
      nat :: RNat n -> Nat

This let's us go in the other direction.. but that doesn't help us! In
fact there's no obvious way to propagate runtime values back into the
types. We're stuck.

## GHC with Iron Dentures

Now, if we could add some magical extension to GHC could we write
something like above program? Yes of course! The key idea is to not
reflect up our types with data kinds, but rather just allow the values
to exist in the types on their own.

For these I propose two basic ideas

 1. A special reflective function type
 2. Lifting expressions into types

For our special function types, we allow the return *type* to use the
supplied *value*. These are called pi types. We'll give this the
following syntax

    (x :: A) -> B x

Where `A :: *` and `B :: Nat -> *` are some sort of type. Notice that
that `Nat` in `B`'s kind isn't the data kind promoted version, but
just the goodness to honest normal value.

Now in order to allow `B` to actually make use of it's supplied value,
our second idea let's normal types be indexed on values! Just like how
GADTs can be indexed on types. We'll call these GGADTs.

So let's define a new version of `RNat`

    data RNat :: Nat -> * where
      RZ : RNat Z
      RS : RNat n -> RNat (S n)

This looks quite familier to what we had before, but our intentions
are different now. Those `Z`'s and `S`'s are meant to represent actual
values, not members of some kind.

Because we can depend on normal values, we don't even have to use our
simple custom natural numbers.

    data RInt :: Int -> * where
      RZ :: RInt 0
      RS :: RInt n -> RInt (1 + n)

Notice that we allowed our types to call functions, like `+`. This can
potentially be undecidable, something that we'll address later.

Now we can write our function with a combination of these two ideas

    toRInt :: (n :: Int) -> RInt n
    toRInt 0 = RZ
    toRInt n = RS (toRInt $ n - 1)

Notice how we used pi types to change the return type dependent on the
input *value*. Now we can feed this any old value, including ones we
read from standard input.


``` haskell
    main = print . toInt $ plus' <$> fmap toRInt readLn <*> fmap toRInt readLn
```

Now, one might wonder how the typechecker could possibly know how to
handle such things, after all how could it know what'll be read from
stdin!

The answer is that it doesn't. When a value is reflected to the type
level we can't do anything with it. For example, if we had a type like

    (n :: Int) -> (if n == 0 then Bool else ())

Then we would have to pattern match on `n` at the value level to
propagate information about `n` back to the type level.

If we did something like

    foo :: (n :: Int) -> (if n == 0 then Bool else ())
    foo n = case n of
      0 -> True
      _ -> ()

Then the typechecker would see that we're matching on `n`, so if we
get into the `0 -> ...` branch then `n` must be `0`. It can then
reduce the return type to `if 0 == 0 then Bool else ()` and finally
`Bool`.

This means that when we use pi types we often have to pattern match on
our arguments in order to help the typechecker figure out what's going
on.

To make this clear, let's play the typechecker for this function

    p :: (n :: Int) -> (m :: Int) -> RInt (n + m)
    p 0 m = toRInt m
    p n m = case

first we 


[kind-exp]: /posts/2014-02-10-types-kinds-and-sorts.markdown
