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
ML. We have application with `App`, lambda expressions (using DB
indices) and letrec.

In addition to the staples, we have tagged unions with `Con`. In
Haskell were we'd say `Just x`, in this language we'd say
`Con tagForJust [x]`. Since JavaScript has records lying around we
incorporate them into our language as well. We can project fields out
of record with `Proj`.

Like any good functional language we have pattern matching. We can
match literals as hinted before. Additionally we can pattern match on
tagged constructors. Since we're using DB variables rather than actual
names we annotate the pattern match with arity of the constructor. A
clever person would bake this arity right into the `Tag`, but I am not
them.

The observant reader will have noted the `Maybe Closure` decorating a
lambda. Indeed this will become relevant later. A closure supposed to
represent the set of free variables our term contains. Since are our
local variables are DB, our closure is just a list of integers.

``` haskell
    type Closure = [Int]

    data Bind = Bind { closure :: Maybe Closure
                     , body    :: Expr }
              deriving Show
```

With this in mind the binding for our letrec's should make sense as
well. They'll need to be closured later on so they also have an
optional annotation.

Finally, each top level is a name, the number of arguments it expects,
and the body.

``` haskell
    data Decl = TopLevel Name Int Expr
              deriving Show
```

## Saturation

Saturation traverses our AST and expands the application of primitive
operations so that everything is fully applied. In order to do this it
occasionally must eta-expand things. In order to do this we recurse
across the AST. In most places we just proceed on subterms and leave
everything alone. Indeed, the only interesting part of the code is 3 cases.

``` haskell
    App (App p@PrimOp{} l) r -> App (App p $ go l) (go r)
    App p@PrimOp{} r -> Lam Nothing $ App (App p (succExpr 1 $ go r)) (Var 0)
    p@PrimOp{} -> Lam Nothing . Lam Nothing $ App (App p $ Var 1) (Var 0)
```

If we see a primop applied twice, we can leave it alone, otherwise we
have to saturate it by adding the appropriate amount of lambdas to eta
expand it. One interesting thing here is that in the process of eta
expanding we introduce a new binder. This could potentially mess up
the `r` when we're dealing with a primop applied to one argument. In
order to fix this we call `succExpr`. This bumps all free DB variables
by a certain amount. In this case we introduce one binder so we bump
everything by 1.

I leave `succExpr`s (tedious) implementation to the curious reader.

## Lifting

The next pass is the biggest simplification of AST. We "flatten" out
expressions so that rather than having a deeply nested set of
applications and lambdas, we have a mess of letrecs.

This transformation is captured by traversing the AST and making two
modifications

     App f e -> LetRec [Bind Nothing $ succExpr 1 e] $ App (succExpr 1 f) (Var 0)
     Lam c f -> LetRec [Bind c $ Lam c f] (Var 0)

From here we need to propagate all these letrecs up through
expressions. To do this `Lift.hs` defines a bunch of `merge*`
functions. These exist to propagate letrecs through a nonbinding
constructs.

To give an idea of how these works, here's how we propagate things
through an `App`.

``` haskell
    mergeApp :: Expr -> Expr -> Expr
    mergeApp l@(LetRec lbs _) r@(LetRec rbs _) =
      let LetRec lbs' e  = succExpr (length rbs) l
          LetRec rbs' e' = succExprFrom (negate $ length rbs) (length lbs) r
      in LetRec (lbs' ++ rbs') (App e e')
    mergeApp (LetRec bs e) r =
      LetRec bs $ App e (succExpr (length bs) r)
    mergeApp l (LetRec bs e) =
      LetRec bs $ App (succExpr (length bs) l) e
    mergeApp l r = App l r
```

The most complicated case occurs when both the left and the right have
letrecs. We need to merge these together, but we have to carefully
escape all the bound variables that occur on both the left and the
right side.

Now we intend to stick the right bindings later. That means that
they'll be bound as `length lbs, length lbs + 1, ...`. We want to bump
all variables on the right side then by `length rbs`. To do this we
use `succExprFrom`. This is just like `succExpr` but let's us bump all
variables later than a specific threshold. We set this to
`- length rbs` as an awful hack to ensure we bump the variables bound
in that letrec as well.

On the left side the variables bound in the new letrec will have the
same indice as before. We just need to make sure that all free
variables outside of it are incremented to dodge the new bindings we
introduce. To achieve this we `succExpr (length rbs)` everything on
the left hand side.

After all of this all we need do is combine everything with `++` and
`App`. It is around this time I lost faith in type systems and began
worshipping Cthulu. Do you see what DeBruijn indices do to people?

Now we can actually discuss how lambda lifting and argument flattening
work. Since they actually are quite similar I'll just show lambda
lifting.

``` haskell
    lambdaLift :: Expr -> Expr
    lambdaLift = \case
      Var i -> Var i
      Global n -> Global n
      Lit l -> Lit l
      Con t es -> mergeCon t (map lambdaLift es)
      PrimOp p -> PrimOp p
      Record rs -> mergeRecord (map (fmap lambdaLift) rs)
      Proj e n -> case lambdaLift e of
                   LetRec bs e' -> LetRec bs (Proj e' n)
                   e' -> Proj e' n
      LetRec bs e -> LetRec (map liftB bs) (lambdaLift e)
      App l r -> mergeApp (lambdaLift l) (lambdaLift r)
      Case e alts -> Case (lambdaLift e) (map (fmap lambdaLift) alts)
      Lam c e -> LetRec [Bind c . Lam c $ skipLambdas e] (Var 0)
      where liftB (Bind c e) = Bind c (skipLambdas e)
            skipLambdas (Lam c e) = Lam c (skipLambdas e)
            skipLambdas e = lambdaLift e
```

The first few cases just recurse. Occasionally we need to merge the
letrecs we generate together with the `merge` functions we just
discussed.

Things start to be interesting with `Lam`. When we get a lambda we
lift it into a letrec and recur on our bound term. The really
interesting bit is how we recur. In particular, it's okay to have a
lambda immediately following another. For this we have
`skipLambdas`. It cheerfully recurses over all lambdas until we
finally get something that isn't one. Then we go back to using
`lambdaLift`.

We're very close to the host language for an STG machine. All we that
we need now is a few annotations.

## Closure Annotations

Next up is closure annotating. The basic idea is as we go through the
expression we look at each binder and annotate it with the free
variables it closes over.

First things first, we need to figure out what those free variables are.

``` haskell
    freeVars :: Expr -> S.Set Int
    freeVars = \case
      Var i -> S.singleton i
      Record rs -> foldMap freeVars (fmap snd rs)
      Proj e _ -> freeVars e
      LetRec binds b -> prune (length binds)
                        $ foldMap freeVars (b : map body binds)
      Lam _ e -> prune 1 $ freeVars e
      App l r -> freeVars l `S.union` freeVars r
      Case e alts -> freeVars e `S.union` foldMap freePat alts
      Con _ es -> foldMap freeVars es
      _ -> S.empty
      where prune n = S.map (subtract n) . S.filter (>= n)
            freePat (LitPat _, e) = freeVars e
            freePat (WildPat, e) = freeVars e
            freePat (ConPat _ i, e) = prune i $ freeVars e
```

For this we have `freeVars`. We traverse the expression and `S.union`
free variables together at recurrences. What's particularly fun is
when we get to a binder. We reduce all the free variables under it by
the amount of free variables bound. Since negative DB variables don't
make sense, we filter all those out.

Now with this in hand closure annotation is quite straightforward.

``` haskell
    annClos :: Expr -> Expr
    annClos = \case
      Record ns -> Record (map (fmap annClos) ns)
      Proj e n -> Proj (annClos e) n
      LetRec binds e -> LetRec (map annB binds) (annClos e)
      l@(Lam _ e) -> Lam (clos l) (annClos e)
      App l r -> App (annClos l) (annClos r)
      Case e bs -> Case (annClos e) (map (fmap annClos) bs)
      Con t es -> Con t (map annClos es)
      e -> e
      where clos = Just . S.toList . freeVars
            annB (Bind _ e) = Bind (clos e) (annClos e)
```

This proceeds much as you would expect, recursing and occasionally
calling `freeVars` to produce annotations.

After this pass we're ready for conversion into STG land.

## STGify

Before we get started, we're starting to go into how f2js implements
it's peculiar little spineless tagless G-machine. It'd be helpful if
you knew what that meant. I [did write about them earlier][stg] if
you're not familiar.

Now with our foray into this brave new world we get a new AST.

``` haskell
    data UpdateFlag = Update | NoUpdate

    data Decl = Decl { declName :: Name
                     , declClos :: Closure }

    data Closure = Closure { closFlag :: UpdateFlag
                           , closClos :: [Name]
                           , closArgs :: [Name]
                           , closBody :: Either String SExpr }

    data Lit = String String | Double Double | Record [(Name, Atom)] | Bool Bool
    data Atom = NameAtom Name | LitAtom Lit

    data Pat = LitPat Lit
             | WildPat
             | ConPat Tag [Name]

    data SExpr = Let [Decl] SExpr
               | App Name [Atom]
               | Con Tag [Atom]
               | Prim PrimOp [Atom]
               | Case SExpr [(Pat, SExpr)]
               | Lit Lit
               | Var Name
               | Proj SExpr Name
```

Sorry to drop all this code on you. But most of it should be at least
familiar. The biggest difference is everything is closure oriented
now. Functions are represented as closures. Bindings are represented
with closures. Even declarations are just closures. Additionally, this
AST is a bit simpler. Application, primops, and constructors can only
be applied to literals and variables, atoms.

These two factors combined make this AST excellent for code
generation, but first we need to actually map the AST we have into the
AST we want.

Another thing we have to what out for is the names. Where before we
had all these nice DB variable names we now have actual `Name`s. To
help with generating them we once again enlist
[`monad-gen`][monad-gen].

I'll elided a lot of the boring bits of conversion and wanted to focus
on two cases, `LetRec` and `App`. For `App` we have a function called
`appChain` who's job it is to try to find a chain of `f a b c`. It
looks like this.

``` haskell
    appChain :: [Name] -> A.Expr -> Maybe (Either Name PrimOp, [S.Atom])
    appChain ns = go []
      where go as (A.App l r) = go (expr2atom ns r : as) l
            go as (A.Var i) = Just (Left $ ns !! i, as)
            go as (A.Global n) = Just (Left n, as)
            go as (A.PrimOp p) = Just (Right p, as)
            go _ _ = Nothing
```

App chain folds left across the expression gather up arguments until
it either reaches a name or a primop. If we find anything that *not* a
name or a primop we know we're not at an app chain so we return
nothing. Additionally, to unroll names we have a list of names where
the name at indice `i` is the name for `Var i`.

Now we plug this into our conversion function `expr2sexpr` with

``` haskell
    e | Just (op, atoms) <- appChain ns e ->
                return $ case op of
                Right p -> S.Prim p atoms
                Left n -> S.App n atoms
```

Notice how the pattern guards automatically handle the process of
figuring out whether some is or isn't a pattern. It's a useful
extension.

Next up is letrec. For this we have a function that takes a `Bind` and
gives us back a closure. Here it is

``` haskell
    bind2clos ns (A.Bind (Just c) e, i) = do
                  (body, args) <- unwrapLambdas e
                  body' <- expr2sexpr (args ++ ns) body
                  let flag = if null args then S.Update else S.NoUpdate
                  return S.Decl { S.declName = ns !! i
                                , S.declClos =
                                  S.Closure { S.closFlag = flag
                                            , S.closClos = map (ns !!) c
                                            , SannotatedArgs = args
                                            , S.closBody = Right body' }}
```

Here `ns` is once again our collection of free variables. In order to
keep track of where we are in the `letrec` each `Bind` comes annotated
with `i`, it's position from the start of the list.

Next we unwrap any lambdas contained inside the bind expression. This
function generates fresh variables so it needs to be inside the `Gen`
monad. It's very similar to `appChain`.

Next we need to figure out whether or not we can update a closure. For
now the simple rule that function closures cannot be updated,
everything else can be suffices. This means that `\x -> x + 1`
wouldn't ever be updated, but `(\x -> x + 1) 1` would be.

Finally, we recurse on the body and construct the appropriate
`S.Decl`. For now we just use `!!` to grab the appropriate
variables. This should be fixed soon.

With `bind2clos` letrec is pleasantly straightforward.

``` haskell
      A.LetRec bs e -> do
        ns' <- (++ ns) <$> replicateM (length bs) gen
        S.Let <$> mapM (bind2clos ns') (zip bs [0..]) <*> expr2sexpr ns' e
```

We generate or fresh stack of variables and recur essentially.

## Code Generation

Unfortunately `CodeGen.hs` is a reasonably large file (~300 lines)
which hints I really ought to split this out into an abstract machine
represented with a normal AST and *then* codegen, but things are still
passable at the moment.

This is where we actually start spitting JS. In order to do this with
use [js-good-parts][js]. The basic idea is the same as outlined in the
STG post I linked to above. We push arguments onto the argument stack,
push continuations onto the continuation stack, and push evaluated
results onto the evaluated stack. In actuality we could cleverly turn
this into one structure, but I haven't done this yet.

It's easiest to handle this code top down, so here's the driver for
the whole thing

``` haskell
    expr :: SExpr -> [J.Stmt]
    expr = \case
      Var n -> [enter (J.ExprName $ jvar n)]
      App n as -> app (jvar n) (map atom as)
      Prim p [l, r] -> primOp p (atom l) (atom r)
      Proj e n -> pushCont (projCont $ jvar n) : expr e
      Lit l -> [enter . mkLit $ lit l]
      Con t as -> [enter $ con t (map atom as)]
      Let ds e -> [letrec (map compileClos ds) (expr e)]
      Case e alts -> (pushCont . matchCont $ map (fmap fnBody) alts) : expr e
```

Going through this branch by branch, when we get a variable we convert
it to it's JS equivalent and enter its closure. When we have an
application we call `app` which pushes all the atoms onto the arg
stack and jump into the variable's closure.

The story is a bit different for primops since we need to evaluate them.

## The Runtime System
## Future Work
## Wrap Up

[github]: http://github.com/jozefg/f2js
[db-style]: http://www.wikiwand.com/en/De_Bruijn_index
[monad-gen]: http://hackage.haskell.org/package/monad-gen
[stg]: /posts/2014-10-28-stg.html
[js]: http://hackage.haskell.org/package/js-good-parts
