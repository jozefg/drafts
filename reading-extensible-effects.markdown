---
title: Examining Hackage: extensible-effects
---

I had a few people tell me after my [last post][last-post] that they would enjoy a
write up on reading [extensible-effects][ee-pk] so here goes.

I'm going to document my process of reading through and understanding
how extensible-effects is implemented. Since this is a fairly large
library (about 1k) of code, we're not going over *all* of it. Rather
we're just reviewing the core modules and enough of the extra ones to
get a sense for how everything is implemented.

If you're curious or still have questions, the modules that we don't
cover should serve as a nice place for further exploration.

## Which Modules

extensible-effects comes with quite a few modules, my grep query
reveals

    $ find src -name "*.hs"
      src/Data/OpenUnion1.hs
      src/Control/Eff/Reader/Strict.hs
      src/Control/Eff/Reader/Lazy.hs
      src/Control/Eff/Fresh.hs
      src/Control/Eff/Cut.hs
      src/Control/Eff/Exception.hs
      src/Control/Eff/State/Strict.hs
      src/Control/Eff/State/Lazy.hs
      src/Control/Eff/Writer/Strict.hs
      src/Control/Eff/Writer/Lazy.hs
      src/Control/Eff/Coroutine.hs
      src/Control/Eff/Trace.hs
      src/Control/Eff/Choose.hs
      src/Control/Eff/Lift.hs
      src/Control/Eff.hs
      src/Control/Eff/Reader/Strict.hs

Whew! Well I'm going to take a leap and assume that extensible-effects
is similar to the mtl in the sense that there are a few core modules,
an then a bunch of "utility" modules. So there's `Control.Monad.Trans`
and then `Control.Monad.State` and a bunch of other implementations of
`MonadTrans`.

If we assume extensible-effects is formatted like this, then we need
to look at

  1. Data.OpenUnion1
  2. Control.Monad.Eff

And maybe a few other modules to get a feel for how to use these
two. I've added `Data.OpenUnion1` because it's imported by
`Control.Monad.Eff` so is presumably important.

Since `Data.OpenUnion1` is at the top of our dependency DAG, we'll
start with it.

## Data.OpenUnion1


