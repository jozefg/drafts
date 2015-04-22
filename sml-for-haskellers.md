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

Common types:

    Int | Integer    | Char | Bool | String | Double | (A,  B,  C)
    int | IntInf.int | char | bool | string | real   | (A * B * C)

Common values:

    1 | 'a'  | True | "hello" | 3.14 | (1, 2, 3)
    1 | #'a' | true | "hello" | 3.14 | (1, 2, 3)

Type variables:

    a  -> a
    'a -> 'a

Function application:

    f x y z
    f x y z

Lambdas:

    \x -> ...
    fn x => ...

If:

    if True then 1 else 0
    if true then 1 else 0

Short circuiting operators:

    True && False       False || False
    true andalso false  false orelse false

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

Type synonyms:

    type Three a b = (a, a, b)
    type ('a, 'b) three = 'a * 'a * 'b

Data types:

    data List a = Cons a (List a) | Nil
    datatype 'a list = Cons of 'a * 'a list | Nil

Type annotations:

    f :: a -> a
    f x = x

    fun f (x : 'a) : 'a = x

Type annotations for expressions:

    (1 + 1 :: Int)
    (1 + 1 :  int)

Let bindings:

    let x = 1     in x + x
    let val x = 1 in x + x end

Declare a new mutable reference:

    newIORef True
    ref true

Modify a mutable reference:

    setIORef r False
    r := false

Read a mutable reference:

    readIORef r
    ! r

## What Is SML Missing


Aside from the obvious things, like SML being strict (so it's missing pervasive
lazy evaluation) the biggest gap I stumble across in SML is the lack of higher
kinded polymorphism,

    data Fix f = Fix (f (Fix f))
    datatype 'f fix = Fix of ('f fix) 'f

Even applying a type variable is a syntax error!

Aside from this, SML doesn't have guards, nor a lot of syntactic sugar that
Haskell has. A nice exception to this is lambda cases, which is written

    fn 0 => 1
     | 1 => 2
     | n => 0

Additionally, SML doesn't have significant identation which means that it's

## What Does SML Have

Of course SML has actual modules. I've
[explained a bit about them earlier](modules). This alone is reason enough to
write some ML. Additionally, SML has a saner notion of records. Records are a
type in and of themselves. This means we can have something like

``` sml
    type coord = {x : int, y : int}
```

However, since this is just a type synonym we don't actually need to declare
it. Accessors are written `#x` to access the field `x` from a record. SML
doesn't have a very advanced record system so `#x` isn't typeable. It's
overloaded to access a field from some record and the concrete record must be
inferrable from context. This often means that while we *can* have free floating
records, the inference woes make us want to wrap them in constructors like so

    data coord = Coord of {x : int, y : int}

This has the nice upshot that record accessors aren't horrible broken with
multiple constructors. Let's say we had

    datatype user = Person {firstName : string, lastName : string}
                  | Robot  {owner : string, guid : int}

We can't apply `#firstName` to an expression of type `user`. It's ill-typed
since `user` isn't a record, it has a constructor which *contains a
record*. In order to apply `#firstName` we have to pattern match first.

Also, modules are freaking amazing, have I mentioned modules yet?

[ocaml-for-haskellers]: http://blog.ezyang.com/2010/10/ocaml-for-haskellers/
[adams-guide]: http://adam.chlipala.net/mlcomp/
[modules]: /posts/2015-01-08-modules.html
