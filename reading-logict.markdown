---
title: Examining Hackage: logict
---

One of my oldest habits with programming is reading other people's
code. I've been doing it almost since I started programming. For the
last two years that habit has been focused on Hackage. Today I was
reading the source code to the "logic programming monad" provided by
[`logict`][logict] and wanted to blog about how I go about reading new
Haskell code.

This time the code was pretty tiny, `find . -name *.hs | xargs wc -l`
reveals two files with just under 400 lines of code! `logict` also
only has two dependencies, base and the mtl!

## Setting Up

It's a lot easier to read this post if you have the source for logict
on hand. To grab it, use `cabal get`. My setup is something like

    ~ $ cabal get logict
    ~ $ cd logict-0.6.0.2
    ~/logict-0.6.0.2 $ cabal sandbox init
    ~/logict-0.6.0.2 $ cabal install --only-dependencies
    
## Poking Around

I'm somewhat ashamed to admit that I use pretty primitive tooling for
exploring a new codebase, it's `grep` and `find` all the way! If you
use a fancy IDE, perhaps you can just skip this section and take a
moment to sit back and feel high-tech.

First things first is to figure out what Haskell files are here. It
can be different than what's listed on Hackage since often libraries
don't export external files.

    ~/logict-0.6.0.2 $ find . -name *.hs
      ./dist/build/autogen/Paths_logict.hs
      ./Control/Monad/Logic.hs
      ./Control/Monad/Logic/Class.hs

Alright, there's two source file and one sitting in dist. The dist one
is almost certainly just cabal auto-gened stuff that we don't care
about.

It also appears that there's no `src` directory and every module is
publicly exported! This means that we only have two modules to worry
about.

The next thing to figure out is which to read first. In this case the
choice is simple: greping for imports with

    grep "import" -r Control

reveals that `Control.Monad.Logic` imports `Control.Monad.Logic.Class`
so we start with `*.Class`.

## Reading `Control.Monad.Logic.Class`

Alright! Now it's actually time to start reading code.

The first thing that jumps out is the export list

    module Control.Monad.Logic.Class (MonadLogic(..), reflect, lnot) where

Alright, so we're exporting everything from a class `MonadLogic`, as
well as two functions `reflect` and `lnot`. Let's go figure out what
`MonadLogic` is.

    class (MonadPlus m) => MonadLogic m where
      msplit     :: m a -> m (Maybe (a, m a))
      interleave :: m a -> m a -> m a
      (>>-)      :: m a -> (a -> m b) -> m b
      ifte       :: m a -> (a -> m b) -> m b -> m b
      once       :: m a -> m a

The fact that this depends on `MonadPlus` is pretty significant. Since
most classes don't require this I'm going to assume that it's fairly
key to either the implementation of some of these methods or to using
them. Similar to how `Monoid` is critical to `Writer`.

The docs make it pretty clear what each member of this class does

  - `msplit`

    Take a local computation and split it into it's first result and
    another computation that computes the rest.

  - `interleave`

    This is the key difference between `MonadLogic` and
    `[]`. `interleave` gives fair choice between two computation. This means
    that every result that appears in finitely many applications of
    `msplit` for some `a` and `b`, will appear in finitely many
    applications of `msplit` to `interleave a b`.

  - `>>-`

     `>>-` is similar to `interleave`. Consider some code like

          (a >>= k) `mplus` (b >>= k)

     This is equivalent to `mplus a b >>= k`, but has different
     characteristics since `>>=` might never terminate. `>>-` is
     described as "considering both sides of the disjunction".

     I have absolutely no idea what that means.. hopefully it'll be
     clearer once we look at some implementations.

  - `ifte`

     This is the equivalent of Prolog's soft cut. We poke a logical
     computation and if it *can* succeed at all, then we feed it into
     the success computation, otherwise we'll feed return the failure case.

  - `once`

     `once` is clever combinator to prevent backtracking. It will grab
     the first result from a computation, wrap it up and return
     it. This prevents backtracking further on the original
     computation.

Now the docs also state that everything is derivable from
`msplit`. These implementations look like

    interleave m1 m2 = msplit m1 >>=
                        maybe m2 (\(a, m1') -> return a `mplus` interleave m2 m1')

    m >>- f = do (a, m') <- maybe mzero return =<< msplit m
                 interleave (f a) (m' >>- f)

    ifte t th el = msplit t >>= maybe el (\(a,m) -> th a `mplus` (m >>= th))

    once m = do (a, _) <- maybe mzero return =<< msplit m
                return a


The first thing I notice looking at interleave is that it kinda looks
like

    interleave' :: [a] -> [a] -> [a]
    interleave' (x:xs) ys = x : interleave' ys xs
    interleave _ ys       = ys

In fact, it seems correct to think of `msplit` as a fancier version of

    uncons :: [a] -> [(a, [a])]


[logict]: http://hackage.haskell.org/package/logict
