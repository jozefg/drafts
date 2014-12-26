---
title: Examining Hackage: folds
tags: haskell
---

I'd like to go through the `folds` package today. To be honest, I
hadn't actually heard of this one until someone mentioned it to me on
/r/haskell but it looks pretty cool.

It's similar to Gabriel's `foldl` library, but it also seems to
provide a wider suite of folds.

## Poking Around

After grabbing the source and looking at the files we see that `folds`
is actually reasonable large

    ~$ cabal get folds && cd folds-0.6.2 && ag -g "hs$"
        src/Data/Fold.hs
        src/Data/Fold/L.hs
        src/Data/Fold/L'.hs
        src/Data/Fold/Class.hs
        src/Data/Fold/M1.hs
        src/Data/Fold/L1.hs
        src/Data/Fold/R.hs
        src/Data/Fold/Internal.hs
        src/Data/Fold/L1'.hs
        src/Data/Fold/R1.hs
        src/Data/Fold/M.hs
        Setup.lhs
        tests/hlint.hs

One that jumps out at me is `Internal` since it likely doesn't depend
on anything. We'll start there.

## Internal

Looking at the top gives a hint for what we're in for

``` haskell
    {-# LANGUAGE FlexibleContexts #-}
    {-# LANGUAGE UndecidableInstances #-}
    {-# LANGUAGE ScopedTypeVariables #-}
    {-# LANGUAGE DeriveDataTypeable #-}
    module Data.Fold.Internal
      ( SnocList(..)
      , SnocList1(..)
      , List1(..)
      , Maybe'(..), maybe'
      , Pair'(..)
      , N(..)
      , Tree(..)
      , Tree1(..)
      , An(..)
      , Box(..)
      ) where
```

Most of the code here isn't really that exciting. This module seems to
be mostly a bunch of (presumably useful) data types + their instances
for `Foldable`, `Functor`, and `Traversable`. Since all 3 of these are
simple enough you can actually just derive them I'll elide them in
most cases.

First up is `SnocList`, if the name didn't give it away it is a
backwards list (snoc is cons backwards)

``` haskell
    data SnocList a = Snoc (SnocList a) a | Nil
      deriving (Eq,Ord,Show,Read,Typeable,Data)
```

Then we have the boilerplatey instances for `Functor` and
`Foldable`. What's a bit odd is that both `foldl` and `foldMap` are
implemented where we only need `foldl`. Presumably this is because
just `foldMap` gives worse performance but that's a little
disappointing.

Next is `SnocList1` and `List1` which are quite similar.

``` haskell
    data SnocList1 a = Snoc1 (SnocList1 a) a | First a
      deriving (Eq,Ord,Show,Read,Typeable,Data)

    data List1 a = Cons1 a (List1 a) | Last a
```

We also have a couple strict structures. Notice that these cannot be
functors since they break `fmap f . fmap g = fmap (f . g)` (why?). We
have

``` haskell
    data Maybe' a = Nothing' | Just' !a

    data Pair' a b = Pair' !a !b
```

And we have the obvious instance for `Foldable Maybe'` and
`Monoid (a, b)`.
