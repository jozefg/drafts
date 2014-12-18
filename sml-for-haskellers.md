---
title: SML for Haskellers
---

Inspired by ezyang's [OCaml for Haskellers][ocaml-for-haskellers] I
decided to write something similar for SML. If you already know OCaml
I also recommend [Adam Chlipala's guide][adams-guide]

I'll follow mostly the same structure as Edward's article so we'll
have

    {- Haskell -}
    (* SML *)

## What Do They Have in Common

SML and Haskell have quite a lot in common

Function application:

    f x y z
    f x y z

Lambdas:

    \x -> ...
    fn x => ...

Pattern matching

    case x of
     Nothing -> ...
     Just a -> ...

    case x of
       NONE => ...
     | SOME a => ...

Top level functions support pattern matching in both:

     factorial 0 = 1
     factorial n = n * factorial (n - 1)

     fun factorial 0 = 1
       | factorial n = n * factorial (n - 1)

We can have top level patterns in both as well:

     (a, b) = (1, 2)
     val (a, b) = (1, 2)


Type synonyms and tuples:

    type Three a b = (a, a, b)
    type ('a, 'b) three = 'a * 'a * 'b


Data types:

    data List a = Cons a (List a) | Nil
    datatype 'a list = Cons of 'a * 'a list | Nil


## What Is SML Missing

The biggest gap I stumble across in SML is the lack of higher kinded
polymorphism,

    data Fix f = Fix (f (Fix f))
    datatype 'f fix = Fix of ('f fix) 'f

Even applying a type variable is a syntax error!

Aside from this, SML doesn't have guards,

## What Does SML Have



[ocaml-for-haskellers]: http://blog.ezyang.com/2010/10/ocaml-for-haskellers/
[adams-guide]: http://adam.chlipala.net/mlcomp/
