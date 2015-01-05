---
title: Compiling a Lazy Language to JavaScript
tags: haskell, compilers
---

The last couple of days I've been working on a fun little compiler
called [f2js][github]. This compiler maps a lazy, dynamically typed
functional language with records, a few primitives, higher order
functions, and pattern matching to JavaScript. In this post I'd like
to outline how this process works.

You should bear in mind that the goal of f2js is *not* performance. In
fact, it's slow as all hell. I really wanted something to use as a
simple back end for my own hobby compilers. Its performance will
improve as my blood pressure rises.

## From 10,000 Up

`f2js` is broken into a few compilation phases

      Saturation
           ⇓
        Lifting
           ⇓
    Closure Conversion
           ⇓
     Conversion to STG
           ⇓
    Code Generation

From there the small run time system kicks in and we can actually
execute our code. The first three passes all work across the original
AST simplifying small components. Saturation ensures that all
operators are fully applied. Lifting changes nested functions and
complicated expressions into simpler, flatter ones that make heavy use
of letrec. Finally, closure conversion annotates the critical
components of the AST with their closed over variables.

After this we're left with a simplified AST and we convert it a
smaller AST. This makes a lot of assumptions we've validated through
the 3 former passes explicit. This pass also prepares a few things for
code generation.

Last but not least the compiler spits out a bunch of truly horrifying
JS and we're done!

Let's examine each pass in greater detail.

## The AST

In order to properly discuss these passes, should discuss how the AST.

``` haskell
    data Lit = String String
             | Double Double
             | Bool Bool
             deriving Show
```

At the leafs of our AST, we have literals. In keeping with how JS
apparently sees the world, we have strings, doubles and
booleans. These literals serve a double purpose as we see, we can use
them as expressions and patterns.

``` haskell
    data Expr = Var Int
              | Global Name
              | Lit Lit
              | Con Tag [Expr] -- Must be fully saturated
              | PrimOp PrimOp
              | Record [(Name, Expr)]
              | Proj Expr Name
              | LetRec [Bind] Expr
              | Lam (Maybe Closure) Expr
              | App Expr Expr
              | Case Expr [(Pat, Expr)]
              deriving Show

    data Pat = LitPat Lit
             | WildPat
             | ConPat Tag Int
             deriving Show
```

Also at the leaves our AST we have variables. There are two
types. Global variables are tagged with the abstract `Name`. Local
variables are stored [DeBruijn style][db-style]. Aside from this our
expression language is pretty much what you'd expect from a Haskell or
ML. We have tagged unions with `Con`. In Haskell were we'd say
`Just x`, in this language we'd say `Con tagForJust [x]`.

Since JavaScript has records lying around we incorporate them into our
language as well. We can project fields out of record with `Proj`.

``` haskell
    type Closure = [Int]

    data Bind = Bind { closure :: Maybe Closure
                     , body    :: Expr }
              deriving Show

    data Decl = TopLevel Name Int Expr
              deriving Show
```

## Saturation

Saturation traverses our AST and expands the application of primitive
operations so that everything is fully applied. In order to do this it
occasionally must eta-expand things.

The critical portion of the code is

``` haskell
    App (App p@PrimOp{} l) r -> App (App p $ go l) (go r)
    App p@PrimOp{} r -> Lam Nothing $ App (App p (succExpr 1 $ go r)) (Var 0)
    p@PrimOp{} -> Lam Nothing . Lam Nothing $ App (App p $ Var 1) (Var 0)
```

## Lifting
## Closure Annotations
## STGify
## Code Generation
## The Runtime System
## Things I Dislike About `f2js`
## Wrap Up

[github]: http://github.com/jozefg/f2js
[db-style]: http://www.wikiwand.com/en/De_Bruijn_index
