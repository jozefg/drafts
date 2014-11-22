---
title: Let's Build a Dependently Typed Language
tags: haskell, types
---

Just yesterday I learned that my clever trick for writing a type
checker actually has a proper name: bidirectional type checking. In
this post I'll explain what exactly that is and we'll use it to write
a few fun type checkers.

First of all, let's talk about one of the fundamental conflicts when
designing a statically typed language: how much information need we
demand from the user? Clearly we can go too far in either
direction. Even people who are supposedly against type inference
support at least *some* inference. I'm not aware of a language that
requires you to write something like

    my_function((my_var : int) + (1 : int) : int) : string

Clearly inferring the types of some expressions are necessary. On the
other hand, if we leave out *all* type annotations then it becomes a
lot harder for a human reader to figure out what's going on! I at
least, need to see signatures for top level functions or I become
grumpy.

So inside a type checker we always have two sort of processes

 1. I know this must have the type T, I'll check to make sure this is
    the case
 2. I have no idea what the type of this expression is, I'll examine
    the expression to figure it out

In a bidirectional type checker, we acknowledge these two phases by
explicitly separating the type checker into two functions

``` haskell
    inferType :: Expr -> Maybe Type
    checkType :: Type -> Expr -> Maybe ()
```

It turns out that a technique like this is surprisingly robust. It
handles everything from subtyping to simple dependent types! To see
how this actually plays out I think it'd be best to just dive in and
do something with it.

## Laying Out Our Language

Now when we're building a bidirectional type checker we really want
our AST to explicitly indicate inferrable vs checkable types. Clearly
the parser might not care so much about this distinction, but prior to
type checking it's helpful to create this polarized tree.

For a simple language you can imagine

``` haskell
    data Ty = Bool
            | Arr Ty Ty
            deriving(Eq, Show)

    data IExpr = Var Int
               | App IExpr CExpr
               | Annot CExpr Ty
               | If CExpr IExpr IExpr
               | ETrue
               | EFalse

    data CExpr = Lam CExpr
               | CI IExpr
```

This is just simply typed lambda calculus with booleans. We're using
DeBruijn indices so we need not specify a variable for `Lam`. The
`IExpr` type is for expressions we can *infer* types for, while
`CExpr` is for types we can *check*.

Much this isn't checking, we can always infer the types of variables,
inferring the types of lambdas is hard, etc. Something worth noting is
`CI`. For any inferrable type, we can make it checkable by inferring a
type and checking that it's equal to what we expected. This is
actually how Haskell works, GHC is just inferring type without
bothering with your signature and then just checks you were right in
the first place!

Now that we've separated out our expressions, we can easily define our
type checker.

``` haskell
    type Env = [Ty]

    (?!) :: [a] -> Int -> Maybe a
    xs ?! i = if i < length xs then Just (xs !! i) else Nothing

    inferType :: Env -> IExpr -> Maybe Ty
    inferType env (Var i) = env ?! i
    inferType env (App l r) =
      case inferType env l of
       Just (Arr lTy rTy) -> checkType env r lTy >> return rTy
       _ -> Nothing
    inferType env (Annot e an) = checkType env e an >> return an
    inferType _ ETrue = return Bool
    inferType _ EFalse = return Bool
    inferType env (If i t e) = do
      checkType env i Bool
      lTy <- inferType env t
      rTy <- inferType env e
      guard (lTy == rTy)
      return lTy

    checkType :: Env -> CExpr -> Ty -> Maybe ()
    checkType env (Lam ce) (Arr l r) = checkType (l : env) ce r
    checkType env (CI e) t = inferType env e >>= guard . (t ==)
    checkType _ _ _ = Nothing
```

So our type checker doesn't have many surprises in it. The environment
is easy to maintain since DeBruijn indices are easily stored in a list.

Now that we've seen how a bidirectional type checker more or less
works, let's kick it up a notch.

## Type Checking Dependent Types

Type checking a simple dependently typed language is actually not
nearly as bad as you'd expect. The first thing to realize is that
since dependent types have only one syntactic category.

We maintain the distinction between inferrable and checkable values,
resulting in

``` haskell
    data IExpr = Var Int
               | App IExpr CExpr
               | Annot CExpr CExpr
               | ETrue
               | EFalse
               | Star -- New stuff starts here
               | Pi CExpr CExpr
               | Const String
               deriving (Eq, Show, Ord)

    data CExpr = Lam CExpr
               | CI IExpr
               deriving (Eq, Show, Ord)
```

So you can see we've added 3 new expressions, all inferrable. `Star`
is just the kind of types as it is in Haskell. `Pi` is the dependent
function type, it's like `Arr`, except the return type can *depend* on
the supplied value.

For example, you can imagine a type like

``` haskell
    replicate :: (n : Int) -> a -> List n a
```

Which says something like "give me an integer `n` and a value and I'll
give you back a list of length `n`".

Finally, we introduce constants. These are necessary simply because
without them this language is unbelievable boring. Constants would be
defined in the environment and they represent constant, irreducible
terms. You should think of them almost like constructors in
Haskell. For example, one can imagine that 3 constants

``` haskell
    Nat :: Star
    Zero :: Nat
    Succ :: (_ : Nat) -> Nat
```

Which serve to define the natural numbers.

Now an important piece of a type checker is comparing types for
equality, in STLC, equivalent types are syntactically equal so that
was solved with `deriving Eq`. Here we need a bit more
subtlety. Indeed, now we need to check arbitrary expressions for
equality! This is hard. In fact, a "perfect" equivalence decision
procedure is impossible (hint: functions). Instead we'll opt for a
compromise, we'll reduce things as much as possible and then just
check syntactic equality. This means that `if True then a else b`
would equal `a` as we'd hope, but `\x -> if x then a else a`
wouldn't.

Now in order to facilitate this check we'll define a type for fully
reduced expressions. Since we're only interested in checking equality
on these terms we can toss the inferrable vs checkable division out
the window.

``` haskell
    data VConst = CAp VConst Val
                | CVar String

    data Val = VStar
             | VTrue
             | VFalse
             | VConst VConst
             | VArr Val Val
             | VPi Val (Val -> Val)
             | VLam (Val -> Val)
             | VGen Int
```

Now since we have constants we can have chains of application that we
can't reduce, that's what `VConst` is. Notice that this handles the
case of just having a constant nicely.

The value dichotomy uses a nice trick from the "Simple Easy!" paper,
we use HOAS to have functions that reduce *themselves* when
applied. The downside of this is that we need `VGen` to peek inside
the now opaque `VLam` and `VPi`. The idea is we'll generate a unique
`Int` and apply the functions to `VGen i`.

Now in order to conveniently generate these fresh integers I used
`monad-gen` (it's not self promotion if it's useful :). Equality
checker comes to

``` haskell
    -- *Whistle and fidget with hands*
    instance Enum Val where
      toEnum = VGen
      fromEnum _ = error "You're a bad person."

    eqTerm :: Val -> Val -> Bool
    eqTerm l r = runGen $ go l r
      where go VStar VStar = return True
            go VTrue VTrue = return True
            go VFalse VFalse = return True
            go (VArr f a) (VArr f' a') = (&&) <$> go f f' <*> go a a'
            go (VLam f) (VLam g) = gen >>= \v -> go (f v) (g v)
            go (VPi f) (VPi g) = gen >>= \v -> go (f v) (g v)
            go (VGen i) (VGen j) = return (i == j)
            go (VConst c) (VConst c') = case (c, c') of
              (CVar v, CVar v') -> return (v == v')
              (CAp f a, CAp f' a') ->
                (&&) <$> go (VConst f) (VConst f') <*> go a a'
              _ -> return False

            go _ _ = return False
```

Basically we just recurse and return true or false at the leaves.

Now that we know how to check equality of values, we actually need to
map terms into those values. This involves basically writing a little
interpreter.

``` haskell
    data Env = Env { localVar :: [Val]
                   , constant :: M.Map String Val }
    getVal :: Either String Int -> Env -> Maybe Val
    getVal (Left s) (Env _ constant) = M.lookup s constant
    getVal (Right i) (Env localVar _) =
      if i < length localVar then Just (localVar !! i) else Nothing

    inf :: Env -> IExpr -> Val
    inf _ ETrue = VTrue
    inf _ EFalse = VFalse
    inf _ Star = VStar
    inf env (Annot e _) = cnf env e
    inf env (Var i) = fromJust (getVal (Right i) env)
    inf env (Const s) = fromJust (getVal (Left s) env)
    inf env (Pi l r) = VPi (cnf env l)
                       (\v -> cnf env{localVar = v : localVar env} r)
    inf env (App l r) =
      case inf env l of
       VLam f -> f (cnf env r)
       VConst c -> VConst . CAp c $ cnf env r
       _ -> error "Impossible: evaluated ill-typed expression"

    cnf :: Env -> CExpr -> Val
    cnf env (CI e) = inf env e
    cnf env (Lam c) = VLam $ \v -> cnf env{localVar = v : localVar env} c
```

This is a big chunk of code, but most of it is quite boring. The
interesting cases are for `Lam`, `Pi`, and `App`. For `App` we
actually do reductions wherever we can, otherwise we know that we've
just got a constant so we slap that on the front. `Lam` and `Pi` are
basically the same, they wrap the evaluation of the body in a function
and evaluate it based on whatever is fed in. This is critical,
otherwise `App`'s reductions get much more complicated.

This was a bit more work than I anticipated, but now we're ready to
actually write the type checker!
