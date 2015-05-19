---
title: Examining Hackage: Transformers
tags: haskell
---

*In a wild change of audience I'd like to write something aimed folks who are
more on the beginner-intermediate end of the spectrum of learning Haskell.*

In Haskell we organize our programs so that effects are explicitly bounded to
certain areas of our program using types. The most famous example of this is the
`IO` type: all of our favorite interesting functions return things of the form
`IO Foo` instead of `Foo` this sequesters code that does Input and Output
through the types.

In order to handle these opaque `IO` thingies we have a "monadic"
interface. Monads are for a lot of things, not just effects or space suits. I
stubbornly refuse to write an explanation of them though because I'll do an
awful job. However, luckily some other people [have][monad1] [managed][monad2]
[not][monad3] to. Once monads have stopped making your brain hurt (mostly) you
can move on to making it hurt all over again monad transformers! One thing that
I've always found helpful for understanding concepts like these are seeing a
bunch of example. Happily, there's a whole heap of interesting examples of
monads in one of the most used Haskell libraries: `transformers`.

In this post we'll go through (some) of transformers line by line, I hope this
gives a nice account of some widely used Haskell code and a concept like monad
transformers might actually be applied. Honestly, there's a lot of good stuff in
this package so you'll hopefully learn a little more than just that :)

## Getting the Code

In order to follow along this blog post I strongly urge you grab the code for
yourself. It's much more fun that way!

    ~  $ cabal get transformers
    ~  $ cd transformers-0.4.3.0
    .. $ ls
    Control  Data  LICENSE  Setup.hs  changelog  oldsrc  transformers.cabal

So right away as we start looking at the source there's a slightly odd split
between the `Data` and `Control` folders. Both contain a significant amount of
code. Poking around the `Data` folder shows that it's a bunch of modules of the
form `Data.Functor.*`. Since they don't seem to import anything else let's just
start there!

## Data.Functor.*
### Classes.hs

OK, so the first module in this folder alphabetically is

``` haskell
    module Data.Functor.Classes
```

Looking at this module we note that exports several things

``` haskell
    Eq1(..),
    Ord1(..),
    Read1(..),
    Show1(..),
    readsData,
    readsUnary,
    readsUnary1,
    readsBinary1,
    showsUnary,
    showsUnary1,
    showsBinary1,
```

The first 4 things turn out to be type classes, all of which are mirrors of some
much familiar code from `Prelude`. Specifically they take type classes like `Eq`
and define them to operate over type *constructors*, like `[]`. This best seen
with an example

``` haskell
    class Eq1 f where
      eq1 :: (Eq a) => f a -> f a -> Bool
```

So we can say that `Eq1 f` holds if for any type we can compare for equality,
`a`, `f a` can also be compared for equality. A type class like this seems
almost like a reification of an instance of the form

``` haskell
    -- This
    instance Eq1 F where
      ....
    -- Seems morally equivalent to
    instance Eq a => Eq (F a) where
```

Now the next 3 type classes are just like this.

``` haskell
    class (Eq1 f) => Ord1 f where
        compare1 :: (Ord a) => f a -> f a -> Ordering
    class Read1 f where
        readsPrec1 :: (Read a) => Int -> ReadS (f a)
    class Show1 f where
        showsPrec1 :: (Show a) => Int -> f a -> ShowS
```

So `Ord1` requires an instance for `Eq1`, this is just like the "lower"
counterparts where `Ord a` requires `Eq a`. One slightly different thing is the
functions these type classes contain. Most people thing that `Show` for example
only contains the function `show :: a -> String` but this isn't actually the
case. All of `Ord`, `Show`, and `Read` contain several methods and they're
defined in a mutually recursive fashion. The code for `Show` is

``` haskell
    type ShowS = String -> String

    class  Show a  where
      showsPrec :: Int    -- ^ the operator precedence of the enclosing
      show      :: a   -> String
      showList  :: [a] -> ShowS

      showsPrec _ x s = show x ++ s
      show x          = shows x ""
      showList ls   s = showList__ shows ls s

      showList__ :: (a -> ShowS) ->  [a] -> ShowS
      showList__ _     []     s = "[]" ++ s
      showList__ showx (x:xs) s = '[' : showx x (showl xs)
        where
          showl []     = ']' : s
          showl (y:ys) = ',' : showx y (showl ys)

    shows           :: (Show a) => a -> ShowS
    shows           =  showsPrec 0
```

So there are in fact 3 methods in this class, `showPrecs`, `show`, and
`showList`. `showList` is frankly just a hack to deal with the fact that `String
= [Char]` so let's ignore that for a moment. There's `showPrecs` and
`show`. `show` is really just a specific case of `showPrecs`
