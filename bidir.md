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
nearly as bad as you'd expect. The first thing to do is figure out
is what our new types are.

In addition to our old booleans and functions we introduce a new
"dependent function".

``` haskell
    data Ty = Bool
            | Arr Ty Ty
            | Pi Ty Ty
            deriving(Eq, Show)
```
