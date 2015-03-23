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
### Lambda Lifting
### C-With-Expression
### Converting To SSA-ish C
## Wrap Up

[pcf]: http://github.com/jozefg/pcf
[lambdapi]: http://jozefg.bitbucket.org/posts/2014-12-17-variables.html
[bound-docs]: http://hackage.haskell.org/package/bound-1.0.4/docs/Bound.html
[mg]: http://hackage.haskell.org/package/monad-gen
[c-dsl]: http://hackage.haskell.org/package/c-dsl
[hm]: posts/2015-02-28-type-inference.html
