---
title: Grokking recursion-schemes: Part 3
---

This is my third and final post in my series on understanding
recursion-schemes.

As promised, this post will mostly be my attempt to accumulate as many
nice examples of recursion-schemes as possible. Therefore, it's
probably best to read this post one piece at a time, playing with each
example as you go.

Not all of this code is mine; in the interest of being a good one-page
list of examples I've attempted to make this page the transitive
closure of most code examples.

I've linked to original sources/posts where I got the code from in all
these cases. If somethings ever unclear, please yell at me in a
comment and check out what explanation the original author provided.

### Manipulating ASTs

I'll start with an example near and dear to my heart, fiddling with
ASTs using recursion-schemes.

First let's start by defining a simple Lispish language

``` haskell
    data Var = Var String -- In original code this was more structured
    data Lit = SLit String | ILit Int
    data Primop = Plus | Minus | Mult | Div 

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

Now we can actually talk about the real work. First, we handle
shadowing in `LamF` by checking to see whether we have a variable
identical to `var` being bound in the formal parameters, if we do,
then we use the original version of all our subexpressions. If `var`
isn't shadowed we go ahead and use the substituted version.

Finally, the actual substitution just boils down to 

``` haskell
    if v == var then exp else Var v
```

So there's a real life example of paramorphisms.

Now with these two ideas, we can implement beta conversion. In a lisp
compiler, we'd desugar `let` bindings to function applications so
doing this faux-inlining is vital.

``` haskell
    inlineExp :: SExp CPSPrim -> SExp CPSPrim
    inlineExp = cata folder -- Simple inlining, inline only small pure arguments
      where folder (AppF (Lam [v] [e]) [a]) | size e < 30 = subst v a e 
            folder e = embed e
```

Of course, in a real Lisp, we'd have to do more checks to make sure
`e` didn't have any side effects, that we didn't mutate `v`, so on and
so on. This pure-ish Lisp still nicely illustrates how we'd scale up
to a more real language.

## Merge sorting
