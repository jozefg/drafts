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

extensible-effects comes with quite a few modules, my find query
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

So we're starting with Data.OpenUnion1. If the authors of this code
have stuck to normal Haskell naming conventions, that's an open union
of type *constructors*, stuff with the kind `* -> *`.

Happily, this module has an export list so we can at least see what's
public.

``` haskell
    module Data.OpenUnion1( Union (..)
                          , SetMember
                          , Member
                          , (:>)
                          , inj
                          , prj
                          , prjForce
                          , decomp
                          , unsafeReUnion
                          ) where
```

So we're looking at a data type `Union`, which we export everything
for. Two type classes `SetMember` and `Member`, a type operator `:>`,
and a handful of functions, most likely to work with `Union`.

Before we get to anything else, there seems a simple identity 

So let's figure out exactly what this union thing is

    data Union r v = forall t. (Functor t, Typeable1 t) => Union (t v)

So `Union r v` is just a wrapper around some of functor applied to
`v`? This seems a little odd, what's this `r` thing? The docs hint
that it should always be true that `Member t r` should always hold.

`Member` is a type class of two parameters with no members. In fact,
`grep`ing the entire source reveals that the entire definition and
instances for `Member` in this code base is

``` haskell
    infixr 1 :>
    data ((a :: * -> *) :> b)
    
    class Member t r
    instance Member t (t :> r)
    instance Member t r => Member t (t' :> r)
```

So this makes it a bit clearer, `:>` acts like a type level cons and
`Member` just checks for membership!

Now `Union` makes a bit more sense, especially in light of the `inj`
function

``` haskell
    inj :: (Functor t, Typeable1 t, Member t r) => t v -> Union r v
    inj = Union
```

So `Union` takes some `t` in `r` and hides it away in an existential
applied to `v`. Now this is kinda like having a great nested bunch of
`Either`s with every `t` applied to `v`.

Dual to `inj`, we can define a projection from a `Union` to some `t`
in `r`. This will need to return something wrapped in `Maybe` since we
don't know which member of `r` our `Union` is wrapping.

``` haskell
    prj :: (Typeable1 t, Member t r) => Union r v -> Maybe (t v)
    prj (Union v) = runId <$> gcast1 (Id v)
```

`prj` does some evil `Typeable` casts, but this is necessary since
we're throwing away all our type information with that
existential. That `Id` `runId` pair is needed since `gcast1` has the
type

``` haskell
    -- In our case, `c ~ Id`
    gcast1 :: (Typeable t', Typeable t) => c (t a) -> Maybe (c (t' a))
```

They're just defined as

``` haskell
    newtype Id a = Id { runId :: a }
      deriving Typeable
```

so just like `Control.Monad.Identity`.

Now let's try to figure out what this `SetMember` thing is.

``` haskell
    class Member t r => SetMember set (t :: * -> *) r | r set -> t
    instance SetMember set t r => SetMember set t (t' :> r)
```

This is unhelpful, all we have is the recursive step with no base
case! Resorting to grep reveals that our base case is defined in
`Control.Eff.Lift` so we'll temporarily put this class off until then.

Now the rest of the file is defining a few functions to operate over
`Union`s.

First up is an unsafe "forced" version of `prj`.

``` haskell
    infixl 4 <?>

    (<?>) :: Maybe a -> a -> a
    Just a <?> _ = a
    _ <?> a = a
    
    prjForce :: (Typeable1 t, Member t r) => Union r v -> (t v -> a) -> a
    prjForce u f = f <$> prj u <?> error "prjForce with an invalid type"
```

`prjForce` is really exactly what it says on the label, it's a version
of `prj` that throws an exception if we're in the wrong state of
`Union`.

Next is a way of unsafely rejiggering the type level list that `Union`
is indexed over.

``` haskell
    unsafeReUnion :: Union r w -> Union t w
    unsafeReUnion (Union v) = Union v
```

We need this for our last function, `decom`. This function partially
unfolds our `Union` into an `Either`

``` haskell
    decomp :: Typeable1 t => Union (t :> r) v -> Either (Union r v) (t v)
    decomp u = Right <$> prj u <?> Left (unsafeReUnion u)
```

This provides a way to actually do some sort of induction on `r` by
breaking out each type piece by piece with some absurd case for when
we don't have `a :> b`.

That about wraps up this little `Union` library, let's move on to see
how this is actually used.

## Control.Eff

Now let's actually talk about the core of extensible-effects,
`Control.Eff`. As always we'll start by taking a look at the export
list

``` haskell
    module Control.Eff(
                        Eff (..)
                      , VE (..)
                      , Member
                      , SetMember
                      , Union
                      , (:>)
                      , inj
                      , prj
                      , prjForce
                      , decomp
                      , send
                      , admin
                      , run
                      , interpose
                      , handleRelay
                      , unsafeReUnion
                      ) where
```

So right away we can see that we're exporting stuff `Data.Union1` as
well as several new things, including the infamous `Eff`.

The first definition we come across in this module is `VE`. `VE` is
either a simple value or a `Union` applied to a `VE`!

``` haskell
    data VE r w = Val w | E !(Union r (VE r w))
```

Right away we notice that "pure value or X" pattern we see with free
monads and other abstractions over effects.

We also include a quick function to try to extract a pure value form
`Val`s

``` haskell
    fromVal :: VE r w -> w
    fromVal (Val w) = w
    fromVal _ = error "extensible-effects: fromVal was called on a non-terminal effect."
```

Now we've finally reached the definition of `Eff`!

``` haskell
    newtype Eff r a = Eff { runEff :: forall w. (a -> VE r w) -> VE r w }
```

So `Eff` bears a striking resemblance to `Cont`. There are two
critical differences though, first is that we specialize our return
type to something constructed with `VE r`. The second crucial
difference is that by universally quantifying over `w` we sacrifice a
lot of the power of `Cont`, including `callCC`!

Next in `Control.Eff` is the instances for `Eff`

``` haskell
    instance Functor (Eff r) where
        fmap f m = Eff $ \k -> runEff m (k . f)
        {-# INLINE fmap #-}
    
    instance Applicative (Eff r) where
        pure = return
        (<*>) = ap
    
    instance Monad (Eff r) where
        return x = Eff $ \k -> k x
        {-# INLINE return #-}
    
        m >>= f = Eff $ \k -> runEff m (\v -> runEff (f v) k)
        {-# INLINE (>>=) #-}
```

Notice that these are all really identical to `Cont`s
instances. `Functor` adds a function to the head of the continuation.
`Monad` dereferences `m` and feeds the result into `f`. Exactly as
with `Cont`.

Next we can our primitive function for handling effects

``` haskell
    send :: (forall w. (a -> VE r w) -> Union r (VE r w)) -> Eff r a
    send f = Eff (E . f)
```

I must admit, this tripped me up for a while. Here's how I read it,
"provide a function, which when given a continuation for the rest of
the program expecting an `a`, produces a side effecting `VE r w` and
we'll map that into `Eff`".
