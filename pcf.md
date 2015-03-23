---
title: A Tiny Compiler For A Typed Higher Order Language
tags: compilers, types, haskell
---

Hi folks, the last week or so I was a little tired of schoolwork so I
decided to scratch out some fun code. The end result is an
[extremely small compiler][pcf] for a typed, higher order functional
language called PCF to C. In this post I'll explain attempt to explain
the whole thing, from front to back :)

## What's PCF

First things first, it's important to define the language we're
compiling. The language, PCF, is an extremely small language you
generally find in a book on programming languages, it originates with
Plotkin if I'm not mistaken.

PCF is based around 3 core elements: natural numbers, functions
(closures), and general recursion. There are two constants for
creating numbers, `Zero` and `Suc`. `Zero` is self explanatory and
`Suc e` is the successor of the natural number `e` evaluates to. In
most programming languages this just means `Suc e = 1 + e` but `+`
isn't a primitive in PCF (we can define it as a normal
function).

For functions, we have lambdas like you'd find in any functional
language. Since PCF includes no polymorphism it's necessary to
annotate the function's argument with it's type.

Finally, the weird bit: recursion. In PCF we write recursive things
with `fix x : τ in e`. Here we get to use `x` in `e` and we should
understand that `x` "stands for" the whole expression, `fix ...`. As
an example, here's how we define `+`.

``` haskell
    plus =
          fix rec : nat -> nat -> nat in
            λ m : nat.
            λ n : nat.
              ifz m {
                  Zero  => n
                | Suc x => Suc (rec x n)
              }
```

## Now Let's Compile It

Now compilation is broken up into a bunch of phases and intermediate
languages. Even in this small of a compiler there are 3 (count-em)
languages so along with the source and target language there are 5
different languages running around inside of this compiler. Each phase
with the exception of typechecking is just translating one
intermediate language (IL) into another and in the process making one
small modification to the program as a whole.

### The AST

This compiler starts with an AST, I have no desire to write a parser
for this because parsers make me itchy. Here's the AST

``` haskell
    data Ty = Arr Ty Ty
            | Nat
            deriving Eq

    data Exp a = V a
               | App (Exp a) (Exp a)
               | Ifz (Exp a) (Exp a) (Scope () Exp a)
               | Lam Ty (Scope () Exp a)
               | Fix Ty (Scope () Exp a)
               | Suc (Exp a)
               | Zero
               deriving (Eq, Functor, Foldable, Traversable)
```

What's interesting here is that our AST uses `bound` to manage
variables. Unfortunately there really isn't time to write both a bound
tutorial *and* a PCF compiler one. I've written about using bound
before [here](lambdapi) otherwise you can just check out the official
[docs][bound-docs]. The important bits here are that `Scope () ...`
binds one variable and that `a` stands for the free variables in an
expression. 3 constructs bind variables here, `Ifz` for pattern
matching, `Fix` for recursive bindings, and `Lam` for the
argument. Note also that `Fix` and `Lam` both must be annotated with a
type otherwise stuff like `fix x in x` and `fn x => x` are ambiguous.

### Type Checking

First up is type checking. This should be familiar to most people
we've written a type checker before since PCF is simply typed. We
simply have a `Map` of variables to types. Since we want to go under
binders defined using `Scope` we'll have to use `instantiate`. However
this demands we be able to create fresh free variables so we don't
accidentally cause clashes. To prevent this we use [monad-gen][mg] to
generate fresh free variables.

To warm up, here's a helper function to check that an expression has a
particular type. This uses the more general `typeCheck` function which
actually produces the type of an expression.

``` haskell
    type TyM a = MaybeT (Gen a)

    assertTy :: (Enum a, Ord a) => M.Map a Ty -> Exp a -> Ty -> TyM a ()
    assertTy env e t = (== t) <$> typeCheck env e >>= guard
```

This type checks the variable in an environment (something that stores
the types of all of the free variables). Once it receives that it
compares it to the type we expected and chucks the resulting boolean
into guard. This code is used in places like `Ifz` where we happen to
know that the first expression has the type `Nat`.

Now on to the main code, `typeCheck`

``` haskell
    typeCheck :: (Enum a, Ord a) => M.Map a Ty -> Exp a -> TyM a Ty
    typeCheck _   Zero = return Nat
    typeCheck env (Suc e) = assertTy env e Nat >> return Nat
```

The first two cases for `typeCheck` are nice and straightforward. All
we if we get a `Zero` then it has type `Nat`. If we get a `Suc e` we
assert that `e` is an integer and then the whole thing has the type
`Nat`.

``` haskell
    typeCheck env (V a) = MaybeT . return $ M.lookup a env
```

For variables we just look things up in the environment. Since this
returns a `Maybe` it's nice and easy to just jam it into our `MaybeT`.

``` haskell
    typeCheck env (App f a) = typeCheck env f >>= \case
      Arr fTy tTy -> assertTy env a fTy >> return tTy
      _ -> mzero
```

Application is a little more interesting. We recurse over the function
and make sure it has an actual function type. If it does, we assert
the argument has the argument type and return the domain. If it
doesn't have a function type, we just fail.

```
    typeCheck env (Lam t bind) = do
      v <- gen
      Arr t <$> typeCheck (M.insert v t env) (instantiate1 (V v) bind)
    typeCheck env (Fix t bind) = do
      v <- gen
      assertTy (M.insert v t env) (instantiate1 (V v) bind) t
      return t
```

Type checking lambdas and fixpoints is quite similar. In both cases we
generate a fresh variable to unravel the binder with. We know what
type this variable is supposed to have because we required explicit
annotations so we add that to the map constituting our
environment. Here's where they diverge.

For a fixpoint we want to make sure that the body has the type as we
said it would so we use `assertTy`. For a lambda we infer the body
type and return a function from the given argument type to the body
type.

```
    typeCheck env (Ifz i t e) = do
      assertTy env i Nat
      ty <- typeCheck env t
      v <- gen
      assertTy (M.insert v Nat env) (instantiate1 (V v) e) ty
      return ty
```

For `Ifz` we want to ensure that we actually are casing on a `Nat` so
we use `assertTy`. Next we figure out what type the zero branch
returns and make sure that the else branch has the same type.

All in all this type checker is not particularly fascinating since all
we have are simple types. Things get a bit more interesting with
[polymorphism][hm]. I'd suggest looking at that if you want to see a
more interesting type checker.

### Closure Conversion

Now for our first interesting compilation phase, closure
conversion. In this phase we make closures explicit by annotating
lambdas and fixpoints with the variables that they close over. Those
variables are then explicitly bound in the scope of the lambda. With
these changes, our new syntax tree looks like this

``` haskell
    -- Invariant, Clos only contains VCs, can't be enforced statically due
    -- to annoying monad instance
    type Clos a = [ExpC a]

    data ExpC a = VC a
                | AppC (ExpC a) (ExpC a)
                | LamC Ty (Clos a) (Scope Int ExpC a)
                | FixC Ty (Clos a) (Scope Int ExpC a)
                | IfzC (ExpC a) (ExpC a) (Scope () ExpC a)
                | SucC (ExpC a)
                | ZeroC
                deriving (Eq, Functor, Foldable, Traversable)
```

The interesting parts are the additions of `Clos` and the fact that
the `Scope` for a lambda and a fixpoint now binds an arbitrary number
of variables instead of just one. Here if a lambda or fixpoint binds
`n` variables, the first `n - 1` are stored in the `Clos` and the last
one is the "argument". Closure conversion is thus just the process of
converting an `Exp` to an `ExpC`.

``` haskell
    closConv :: (Show a, Eq a, Ord a, Enum a) => Exp a -> Gen a (ExpC a)
    closConv (V a) = return (VC a)
    closConv Zero = return ZeroC
    closConv (Suc e) = SucC <$> closConv e
    closConv (App f a) = AppC <$> closConv f <*> closConv a
    closConv (Ifz i t e) = do
      v <- gen
      e' <- abstract1 v <$> closConv (instantiate1 (V v) e)
      IfzC <$> closConv i <*> closConv t <*> return e'
```

Most of the cases here are just recursing and building things back up
applicatively. There's the moderately interesting case where we
instantiate the else branch of an `Ifz` with a fresh variable and
*then* recurse, but the interesting cases are for fixpoints and
lambdas. Since they're completely identical we only present the case
for `Fix`.

``` haskell
    closConv (Fix t bind) = do
      v <- gen
      body <- closConv (instantiate1 (V v) bind)
      let freeVars = S.toList . S.delete v $ foldMap S.singleton body
          rebind v' = elemIndex v' freeVars <|>
                      (guard (v' == v) *> (Just $ length freeVars))
      return $ FixC t (map VC freeVars) (abstract rebind body)
```

There's a lot going on here but it boils down into three parts.

 0. Recurse under the binder
 1. Gather all the free variables in the body
 2. Rebind the body together so that all the free variables map to
    their position in the closure and the argument is `n` where `n` is
    the number of free variables.

The first is accomplished in much the same way as in the above
cases. To gather the number of free variables all we need to is use
the readily available notion of a monoid on sets. The whole process is
just `foldMap S.singleton`! There's one small catch: we don't want to
put the argument into the list of variables we close over so we
carefully delete it from the closure. We then convert it to a list
which gives us an actual `Clos a`. Now for the third step we have
`rebind`.

`rebind` maps a free variable to `Maybe Int`. It maps a free variable
to it's binding occurrence it has one here.  This boils down to using
`elemIndex` to look up somethings position in the `Clos` we just built
up. We also have a special case for when the variable we're looking at
is the "argument" of the function we're fixing. In this case we want
to map it to the last thing we're binding, which is just
`length n`. To capture the "try this and then that" semantics we use
the alternative instance for `Maybe` which works wonderfully.

With this, we've removed implicit closures from our language: one
of the passes on our way to C.

### Lambda Lifting

Next up we remove both fixpoints and lambdas from being
expressions. We want them to have an explicit binding occurrence
because we plan to completely remove them from expressions soon. In
order to do this, we define a language with lambdas and fixpoints
explicitly declared in let expressions. The process of converting from
`ExpC` to this new language is called "lambda lifting" because we're
lifting things into let bindings.

Here's our new language.

``` haskell
    data BindL a = RecL Ty [ExpL a] (Scope Int ExpL a)
                 | NRecL Ty [ExpL a] (Scope Int ExpL a)
                 deriving (Eq, Functor, Foldable, Traversable)
    data ExpL a = VL a
                | AppL (ExpL a) (ExpL a)
                | LetL [BindL a] (Scope Int ExpL a)
                | IfzL (ExpL a) (ExpL a) (Scope () ExpL a)
                | SucL (ExpL a)
                | ZeroL
                deriving (Eq, Functor, Foldable, Traversable)
```

### C-With-Expression
### Converting To SSA-ish C
## Wrap Up

[pcf]: http://github.com/jozefg/pcf
[lambdapi]: http://jozefg.bitbucket.org/posts/2014-12-17-variables.html
[bound-docs]: http://hackage.haskell.org/package/bound-1.0.4/docs/Bound.html
[mg]: http://hackage.haskell.org/package/monad-gen
[c-dsl]: http://hackage.haskell.org/package/c-dsl
[hm]: posts/2015-02-28-type-inference.html
