---
title: Slicing λΠ 5 ways
tags: haskell, compilers
———

After my last post, I didn't quite feel like ending there. I was a
little dissatisfied with how binding was handled in the type checker,
the odd blend of HOAS, GUIDs, and DeBruijn variables was... unique.

In the post I explore 5 versions of the same code

 0. The original method
 1. Using `bound` to handle all binding
 2. Using `unbound` to handle all binding
 3. Giving up and just using explicit names
 4. Full HOAS

Fair warning, I've never used `unbound` before and I'm probably using
`bound` in an incredibly backwards way. You've been warned.

## The Original

## `bound`

So my first stab at making this less complicated was with Edward
Kmett's [bound](http://hackage.haskell.org/package/bound). For those
who aren't familiar with this library, it centers around the data type
`Scope`. `Scope b f a` binds variables of type `b` in the structure
`f` with free variables of type `a`.

Further, `f` must be a monad with the type parameter being type of the
variables. Then what `Scope` does is instantiate these variables to `B
b a` which is precisely equivalent to `Either b a`.

What this results in is that each free variable is a different type
from bound ones. `Scope` provides various functions for instantiating
bound variables and abstracting over free ones. All of these functions
use the notion that `>>=` in our terms now corresponds to a
substitution operator. That's `bound` in a nutshell.

It's a bit easier to grok this by example, here's our calculus ported
to use `Scope`

``` haskell
    data Expr a = Var a
                | App (Expr a) (Expr a)
                | Annot (Expr a) (Expr a)
                | ETrue
                | EFalse
                | Bool
                | Star
                | Pi (Expr a) (Scope () Expr a)
                | Lam (Scope () Expr a)
                deriving(Functor, Eq)
```

So the first major difference is that our polarization between
inferrable and checkable terms is gone! This wasn't something I was
happy about, but in order to use `Scope` we need a monad instance and
we can't define two mutually dependent monad instances without a
function from `CExpr -> IExpr`, something that clearly doesn't exist.

Now in addition to just this, we also need a bunch of boilerplate to
define some type class instances for `Scope`'s benefit.

``` haskell
    instance Eq1 Expr where (==#) = (==)
    instance Applicative Expr where
      pure = return
      (<*>) = ap
    instance Monad Expr where
      return = Var
      Var a >>= f = f a
      (App l r) >>= f = App (l >>= f) (r >>= f)
      ETrue >>= _ = ETrue
      EFalse >>= _ = EFalse
      Bool >>= _ = Bool
      Star >>= _ = Star
      Annot l r >>= f = Annot (l >>= f) (r >>= f)
      Pi l s >>= f = Pi (l >>= f) (s >>>= f)
      Lam e >>= f = Lam (e >>>= f)
```

That weird `>>>=` is just `>>=` that works through `Scope`s. It's a
little bit frustrating that we need this somewhat boilerplate-y monad
instance, but I think the results might be worth it.

From here we completely forgo an explicit `Val` type. We're completely
scrapping that whole HOAS and `VConst` ordeal. Instead we'll just
trust `Scope`'s clever `Eq` instance to handle alpha conversion. We do
need to implement normalization though

``` haskell
    type Val = Expr

    nf :: Expr a -> Val a
    nf = \case
      (Annot e t) -> Annot (nf e) (nf t)
      (Lam e) -> Lam (toScope . nf . fromScope $ e)
      (Pi l r) -> Pi (nf l) (toScope . nf . fromScope $ r)
      (App l r) ->
        case l of
         Lam f -> nf (instantiate1 r f)
         l' -> App l' (nf r)
      e -> e
```

What's interestingly different is actual work is shifted from within
the higher order binders we had before into the case expression in
`App`.

It's also worth mentioning the few bound specifics here. `toScope` and
`fromScope` expose the underlying `f (V b a)` that a `Scope` is
hiding. We're then can polymorphically recur (eat your heart out
sml) over the now unbound variables and continue on our way.

Again, notice that I've defined nothing to do with substitution or
scoping, this is all being handled by bound.

Now our actual type checker is still essentially identical. We're
still using `monad-gen` to generate unique variable names, it's just
that now `bound` handles the messy substitution. The lack of
distinction between inferrable, checkable, and normalized terms did
trip me up once our twice though.

``` haskell
    unbind :: (MonadGen a m, Functor m, Monad f) => Scope () f a -> m (a, f a)
    unbind scope = (\a -> (a, instantiate1 (return a) scope)) <$> gen

    unbindWith :: Monad f => a -> Scope () f a -> f a
    unbindWith = instantiate1 . return

    inferType :: Expr Int -> TyM (Val Int)
    inferType (Var i) = asks (M.lookup i) >>= maybe mzero return
    inferType ETrue = return Bool
    inferType EFalse = return Bool
    inferType Bool = return Star
    inferType Star = return Star
    inferType (Lam _) = mzero -- We can only check lambdas
    inferType (Annot e ty) = do
      checkType ty Star
      let v = nf ty
      v <$ checkType e v
    inferType (App f a) = do
      ty <- inferType f
      case ty of
       Pi aTy body -> nf (App (Lam body) a) <$ checkType a aTy
       _ -> mzero
    inferType (Pi t s) = do
      checkType t Star
      (newVar, s') <- unbind s
      local (M.insert newVar $ nf t) $
        Star <$ checkType s' Star

    checkType :: Expr Int -> Val Int -> TyM ()
    checkType (Lam s) (Pi t ts) = do
      (newVar, s') <- unbind s
      local (M.insert newVar (nf t)) $
        checkType s' (nf $ unbindWith newVar ts)
    checkType e t = inferType e >>= guard . (== t)
```

I defined two helper functions `unbind` and `unbindWith` which both
ease the process of opening a scope and introducing a new free
variable. I actually split these off into a
[tiny library](http://github.com/jozefg/bound-gen), but I haven't
uploaded it to hackage yet.



## `unbound`

## Screw It How Bad Can Real Names Be

## HOAS

## Wrap Up

In conclusion, variables suck and that's why all my future software
will be completely point free.
