---
title: Grokking recursion-schemes: Part 3
---

This is my third and final post in my series on understanding
recursion-schemes.

As promised, this post will mostly be my attempt to accumulate as many
nice examples of recursion-schemes as possible. Therefore, it's
probably best to read this post one piece at a time, playing with each
example as you go.

Not all of this code is mine, I've tried to link to original
sources/posts where I got the code from.

### Manipulating ASTs

I'll start with an example near and dear to my heart, fiddling with
ASTs using recursion-schemes.

First let's start by defining a simple Lispish language

``` haskell
    data Var = Var String -- In original code this was more structured
    data Lit = SLit String | ILit Int
    data Primop = Plus | Minus | Mult | Div | Print

    data Exp = Var Var
             | Lam [Var] [Exp]
             | App Exp [Exp]
             | Prim Op
             | Lit Lit

    data ExpF a = VarF Var
                | LamF [Var] [a]
                | AppF a [a]
                | PrimF Op
                | LitF Lit
                deriving Functor

    instance Foldable Exp where
      ...
    instance Unfoldable Exp where
      ...
```

We can start by implementing a simple catamorphism to give us the
"size" of each term.

``` haskell
    size :: Exp -> Integer
    size = cata folder
      where folder (LamF _ exps) = sum exps
            folder (AppF f args) = sum (f : args)
            folder _             = 1
```

Next we can get a bit more adventures and implement substitution as a
paramorphism.

``` haskell
    subst :: Var -> Exp {- Term to substitute -} -> Exp -> Exp
    subst var exp = para folder
      where folder (LamF vars body)       = Lam vars $ if var `elem` vars then map fst body else map snd body
            folder (VarF v)               = if v == var then exp else Var v
            folder (AppF f args)          = App (snd f) (map snd args)
            folder (LitF l)               = Lit l
            folder (PrimF p)              = Prim p
```

Now this looks scary, but let's remember what's going on here. `para`
is giving us two views of all subexpressions, one where we've
substituted all occurrences of `var` for `exp`, and one where we
haven't. These are ordered as `(original, substituted)`.

We want to choose the substituted version as long as `var` is in
scope, so in the `AppF` case we just grab the second, substituted
value with

``` haskell
    App (snd f) (map snd args)
```

Now 
