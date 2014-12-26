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
`Monoid (a, b)`. Now it may seem a little silly to define these types,
but from experience I can say anything that makes strictness a bit
more explicit is wonderfully helpful. Now we can just use `seq` on a
`Pair'` and know that both components will be forced.

Next we define a type for trees. One thing I noticed was the docs
mentioned that this type reflects the structure of a `foldMap`

``` haskell
    data Tree a
      = Zero
      | One a
      | Two (Tree a) (Tree a)
      deriving (Eq,Ord,Show,Read,Typeable,Data)
```

When we `foldMap` each `One` should be an element of the original
collection. From there we can `fmap` with the `map` part of `foldMap`,
and we can imagine traversing the tree and replacing `Two l r` with
`l <> r`, each `Zero` with `mempty`, and each `One a` with a.

So that's rather nifty. On top of this we have `Foldable`,
`Traversable`, and `Functor` instances.

We also have `Tree1` which is similar but elides the `Zero`

``` haskell
    data Tree1 a = Bin1 (Tree1 a) (Tree1 a) | Tip1 a
```

As you'd expect, this implements the same type classes as `Tree`.

Now is where things get a bit weird. First up is a type for reifying
monoids using `reflection`. I actually was thinking about doing a post
on it and then I discovered Austin Seipp has done an
[outstanding one][thoughtpolice]. So we have this `N` type with the
definition

``` haskell
    newtype N a s = N { runN :: a }
      deriving (Eq,Ord,Show,Read,Typeable,Data)
```

Now with reflection there are two key components, there's the type
class instance floating around and a fresh type `s` that keys it. If
we have `s` then we can easily demand a specific instance with
`reflect (Proxy :: Proxy s)`. That's exactly what we do here. We can
create a monoid instance using this trick with

``` haskell
    instance Reifies s (a -> a -> a, a) => Monoid (N a s) where
      mempty = N $ snd $ reflect (Proxy :: Proxy s)
      mappend (N a) (N b) = N $ fst (reflect (Proxy :: Proxy s)) a b
```

So at each point we use our `s` to grab the tuple of monoid operations
we expect to be around and use them in the obvious manner. The only
reason I could imagine doing this is if we had a structure which we
want to use as a monoid in a number of different ways. I suppose we
also could have just passed the dictionary around but maybe this was
extremely ugly. We shall see later I suppose.

Last comes two data types I do not understand at all. There's `An` and
`Box`. The look extremely boring.

``` haskell
    data Box a = Box a
    newtype An a = An a
```

Their instances are the same everywhere as well.. I have no clue what
these are for. Grepping shows they are used though so hopefully this
mystery will become clearer as we go.

## Class

Going in order of the module DAG gives us `Data.Fold.Class.hs`. This
exports two type classes and one function

``` haskell
    module Data.Fold.Class
      ( Scan(..)
      , Folding(..)
      , beneath
      ) where
```

One thing that worries me a little is that this imports `Control.Lens`
which I don't understand nearly as well as I'd like to.. We'll see how
this turns out.

Our first class is

``` haskell
    class Choice p => Scan p where
      prefix1 :: a -> p a b -> p a b
      postfix1 :: p a b -> a -> p a b
      -- | Apply a 'Folding' to a single element of input
      run1 :: a -> p a b -> b
      interspersing :: a -> p a b -> p a b
```

So right away we notice this is a subclass of `Choice` which is in
turn a subclass of [`Profunctor`][profunctors]. `Choice` captures the
ability to pull an `Either` through our profunctor.

``` haskell
    left' :: p a b -> p (Either a c) (Either b c)
    right' :: p a b -> p (Either c a) (Either c b)
```

Note that we can't do this with ordinary profunctors since we'd need a
function from `Either a c -> a` which isn't complete.

Back to `Scan p`. `Scan p` takes a profunctor which apparently
represents our folds. We then can prefix the input we supply, postfix
the input we supply, and run our fold on a single element of
input. This is a bit weird to me, I'm not sure if the intention is to
write something like

``` haskell
    foldList :: Scan p => [a] -> p a b -> b
    foldList [x] = run1 x
    foldList (x : xs) = foldList xs . prefix1 x
```

or something else entirely. Additionally this doesn't really conform
to my intuition of what a scan is. I'd expect a scan to produce all
of the intermediate output involved in folding. At this point, with no
instances in scope, it's a little tricky to see what's supposed to be
happening here.

Additionally, there are a bunch of default-signature based
implementations of these methods if your type implements
`Foldable`. Since this is the next type class in the module let's look
at that and then skip back to the defaults.

``` haskell
    class Scan p => Folding p where
      prefix :: Foldable t => t a -> p a b -> p a b
      prefixOf :: Fold s a -> s -> p a b -> p a b
      postfix :: Foldable t => p a b -> t a -> p a b
      postfixOf :: Fold s a -> p a b -> s -> p a b
      run :: Foldable t => t a -> p a b -> b
      runOf :: Fold s a -> s -> p a b -> b
      filtering :: (a -> Bool) -> p a b -> p a b
```

At this point I looked at a few of the types and my first thought was
"Oh dammit lens.." but it's actually not so bad! The first thing to do
is ignore the `*Of` functions which work across lens's `Fold`
type. There seems to be a nice pair for each "running" function where
it can work across a `Foldable` container or lens's notion of a fold.

``` haskell
      prefix :: Foldable t => t a -> p a b -> p a b
      postfix :: Foldable t => p a b -> t a -> p a b
      run :: Foldable t => t a -> p a b -> b
```

The first two functions let us create a new fold that will accept some
input and supplement it with a bunch of other inputs. `prefix` gives
the supplemental input followed by the new input and `postfix` does
the reverse. We can actually supply input and run the whole thing with
`run`.

All of these are defined with `folded` from lens which reifies a
foldable container into a `Fold`. so `foo = fooOf folded` is the
default implementation for all of these. Now for the corresponding
fold functions I'm reading them as "If you give me a lens to treat `s`
as a container that I can get elements from and a fold, I'll feed the
elements of `s` into the fold."

The types are tricky, but this type class seems to capture what it
means to run a fold across some type of structure.

Now that we've seen how `An` comes in handy. It's used as a single
object `Foldable` container. Since it's newtyped, this should
basically run the same as just passing a single element in.

``` haskell
    prefix1 = prefix . An
    run1 = run . An
    postfix1 p = postfix p . An
```

So a `Scan` here apparently means a fold over a single element at a
time. Still not sure why this is deserving of the name `Scan` but
there you are.

Last but not least we have a notion of dragging a fold through an
optic with `beneath`.

``` haskell
    beneath :: Profunctor p => Optic p Identity s t a b -> p a b -> p s t
    beneath l f = runIdentity #. l (Identity #. f)
```

Those `#.`'s are like `lmap`s but only work when the function we apply
is a "runtime identity". Basically this means we should be able to
tell whether or not we applied the function or just used
`unsafeCoerce` when running the code. Otherwise all we do is set up
our fold `f` to work across `Identity` and feed it into the optic.

## Concrete Implementations

[thoughtpolice]: https://www.fpcomplete.com/user/thoughtpolice/using-reflection
[profunctors]: https://www.fpcomplete.com/user/liyang/profunctors
