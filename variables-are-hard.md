---
title: Cooking λΠ 4 ways
tags: haskell, compilers
———

After my last post, I didn't quite feel like ending there. I was a
little dissatisfied with how binding was handled in the type checker,
the odd blend of HOAS, GUIDs, and DeBruijn variables was... unique.

In the post I explore 5 versions of the same code

 0. The original method
 1. Using `bound` to handle all binding
 2. Using `unbound` to handle all binding
 3. Full HOAS

Fair warning, I've never used `unbound` before and I'm probably using
`bound` in an incredibly backwards way. You've been warned.

There's a lot of code in this post, enough that I think it's worth
hosting the code on its own. You can find it on [github][cooked-pi-github]
and [bitbucket][cooked-pi-bitbucket].

## The Original

I've already described most of the original method
[here][original-tc]. To recap

 1. Values were HOAS
 2. Terms were DeBruijn
 3. To bridge the gap, we had "free constants" randomly generated

The issue I had with this is we almost got the worst of all 3 words!
We were constantly bumping a counter to keep up with the free
constants we needed to generate. We had to muddy up the types of
values with *another* notion of free constants so we could actually
inspect variables under HOAS binders! And finally, we had to do the
painful and tedious substitutions on DeBruijn terms.

On the other hand, if you'd never used any of those binding schemes
together, you too can go triple or nothing and try to understand that
code :)

What I really wanted was to unify how I represented values and
terms. I still wanted a clearly correct notion of equality, but in
this way I could probably dodge at least two of the above.

The obvious thing to do would be to stick with DeBruijn variables and
just instantiate free variables with constants. This is ugly, but it's
moderately less horrible if we use a library to help us with the process.

## `bound`

So my first stab at this approach was with Edward Kmett's
[bound](http://hackage.haskell.org/package/bound). For those who
aren't familiar with this library, it centers around the data type
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
                | C String
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
      C s >>= _ = C s
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
      (Annot e t) -> nf e -- Important, nf'd data throws away annotations
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
    data Env = Env { localVars :: M.Map Int (Val Int)
                   , constants  :: M.Map String (Val Int) }
    type TyM = ReaderT Env (GenT Int Maybe)

    unbind :: (MonadGen a m, Functor m, Monad f) => Scope () f a -> m (a, f a)
    unbind scope = ((,) <*> flip instantiate1 scope . return) <$> gen

    unbindWith :: Monad f => a -> Scope () f a -> f a
    unbindWith = instantiate1 . return

    inferType :: Expr Int -> TyM (Val Int)
    inferType (Var i) = asks (M.lookup i . localVars) >>= maybe mzero return
    inferType (C s) = asks (M.lookup s . constants) >>= maybe mzero return
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
      local (\e -> e{localVars = M.insert newVar (nf t) $ localVars e}) $
        Star <$ checkType s' Star

    checkType :: Expr Int -> Val Int -> TyM ()
    checkType (Lam s) (Pi t ts) = do
      (newVar, s') <- unbind s
      local (\e -> e{localVars = M.insert newVar (nf t) $ localVars e}) $
        checkType s' (nf $ unbindWith newVar ts)
    checkType e t = inferType e >>= guard . (== t)
```

I defined two helper functions `unbind` and `unbindWith` which both
ease the process of opening a scope and introducing a new free
variable. I actually split these off into a
[tiny library](bound-gen), but I haven't
uploaded it to hackage yet.

 1. Code size decreased by ~50 lines
 2. No more explicit substitution
 3. All the annoying plumbing is in the monad instance which is pretty
    mechanical
 4. We did lose the really nice separation of terms we had before
    though :(

I suppose that 4. would be a nonissue for a lot of people who don't
care about bidirectional type checker.

## `unbound`

## HOAS

## Wrap Up

In conclusion, variables suck and that's why all my future software
will be completely point free. I see no downsides.

[original-tc]: /posts/2014-11-22-bidir.md
[bound-gen]: http://github.com/jozefg/bound-gen
[cooked-pi-github]: http://github.com/jozefg/cooked-pi
[cooked-pi-bitbucket]: http://bitbucket.org/jozefg/cooked-pi
