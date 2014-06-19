This is my short-ish tutorial on how to implement closures in
a simple functional language: Foo.

First, some boilerplate.

> {-# LANGUAGE DeriveFunctor, TypeFamilies #-}
> import Control.Applicative
> import Control.Monad.Gen
> import Control.Monad.Writer
> import Data.Functor.Foldable
> import Debug.Trace

To actually run this code, you'll need `Control.Monad.Gen`, which is
part of my monad-gen package. It's just a thin wrapper over the state
monad to give us fresh variable names later on.

Foo will consistent of a few core primops, numbers, and lambdas. In Haskell,
we can represent Foo expressions as

> data Var = Gen Integer | Name String -- Gen for generated variables
>          deriving(Eq, Show)
> 
> data Primop = Plus | Sub | Mult | Div
>             deriving Show
> 
> data Exp = Var Var
>          | App Exp Exp
>          | Lam [Var] Exp
>          | Prim Primop
>          | Lit Int
>          deriving Show

We allow lambdas to have multiple arguments, but we're treating even
them as sugar for the manually curried form, like we do in Haskell.

Now, let's write a little boilerplate to use recursion-schemes.

> data ExpF a = VarF Var
>             | AppF a a
>             | LamF [Var] a
>             | PrimF Primop
>             | LitF Int
>             | PAppF Primop a a
>             deriving Functor
> type instance Base Exp = ExpF
> instance Foldable Exp where
>   project (Var v)      = VarF v
>   project (App l r)    = AppF l r
>   project (Lam vs e)   = LamF vs e
>   project (Prim p)     = PrimF p
>   project (Lit i)      = LitF i
> instance Unfoldable Exp where
>   embed (VarF v)      = Var v
>   embed (AppF l r)    = App l r
>   embed (LamF v e)    = Lam v e
>   embed (PrimF p)     = Prim p
>   embed (LitF i)      = Lit i

Now, programs in Foo are a series of mutually recursive definitions,
just like Haskell. To represent this, we'll write a new type

> data Def = Def Var [Var] Exp
>          deriving Show

Now, on to closure conversion!

As a brief refresher, a closure is a lambda with references to
variables outside of its scope that aren't global variables.

If you write Haskell, then you use closures all the time! In fact
every multiple argument function is closures.

For example

> add x y = x + y

This is really

> add' = \x -> \y -> x + y

Now look at the inner lamda `\ y -> x + y`. here the reference to
x isn't bound in the lambda's argument list, and it's not defined on
the top level. Closures give rise to currying, partial application, and
can even be used to fake message passing stateful objects!

Now the problem is, almost all compilation targets don't support
closures! Or nested functions at all for that matter. Therefore,
one of the many things a compiler for Foo most do is convert all
closures and anonymous functions into normal, top level functions.

To do this, each lambda needs to be retrofitted to take a bunch of extra
arguments. For example our `add` from would become something like

> add'' = \x -> (\x y -> x + y) x

Now each lambda is "closed" and contains no reference to outside variables.

The next step will be to lift our inner lambda to its own top level definition
so that translation to something like C, STG, or LLVM IR would be pretty straight
forward.

> extra  = \x y -> x + y -- Ignore the fact that this desugars to add'' for a moment
> add''' = \x   -> extra x

Obviously, this is not optimal code! We'll talk about how to optimize this later.
For now though, we can write a simple algorithm for closure conversion and lambda
lifting.

First some helper functions and types

> without :: Eq a => [a] -> [a] -> [a]
> without = foldr (filter . (/=)) -- Like \\ but removes all occurrences
> 
> freeVars :: Exp -> [Var] -- Grab all the unbound variables in an expression
> freeVars = cata folder
>   where folder (VarF v)    = [v]
>         folder (AppF l r)  = l ++ r
>         folder (LamF vs e) = e `without` vs
>         folder (PrimF _)   = []
>         folder (LitF _)    = []
> 
> applyTo :: Exp -> [Var] -> Exp
> applyTo e (a : as) = applyTo (App e $ Var a) as
> applyTo e []       = e

Now, the actual closure conversion pass is quite simple

> closConv :: [Var] -> Exp -> Exp
> closConv globals = cata folder
>   where folder (LamF vs e) =
>           let vars = freeVars e `without` (globals ++ vs)
>            in Lam (vars ++ vs) e `applyTo` vars
>         folder e = embed e

Yep, that's it! Now we can take a Foo expression like

> -- testExp foo = foo (\bar -> bar foo)
> testExp = Lam [foo] $ App (Var foo) (Lam [bar] $ App (Var bar) (Var foo))
>   where [foo, bar] = map Name ["foo", "bar"]

And convert it to an expression like

> -- converted foo = foo ((\foo bar -> bar foo) foo)
> converted :: Exp
> converted = closConv [] testExp

Next we can lift everything to its own top level. This
is another small pass.

> type ClosM = WriterT [Def] (Gen Integer)
> liftLam :: Exp -> ClosM Exp
> liftLam = cata folder
>   where folder (AppF l r)    = App <$> l <*> r
>         folder (VarF v)      = return $ Var v
>         folder (PrimF p)     = return $ Prim p
>         folder (LitF i)      = return $ Lit i
>         folder (LamF vs e)  = do
>           fresh <- Gen <$> gen 
>           Def fresh vs <$> e >>= tell . return
>           return $ Var fresh

Now we can chain these two steps together

> eliminateLams :: [Var] -> Def -> [Def]
> eliminateLams globals (Def nm vs e) = Def nm vs e' : defs
>   where (e', defs) = runGenInt . runWriterT . liftLam $ closConv globals e
> 

And that's it!

Now, one simple optimization we can do during this whole process is to mash lambdas
together. Currently, if we had something like

> slow = Lam [Name "a"] $ Lam [Name "b"] $ undefined

This could get lifted into two separate, equivalent, top levels! We saw this
before with `add`. To fix this we can traverse each expression, and lambdas
like these together. This can be done with one simple pass

> smash :: Exp -> Exp
> smash = cata folder
>   where folder (LamF vs (Lam vs' e)) = Lam (vs ++ vs') e
>         folder e                     = embed e

If we plug this into `eliminateLam`, we end up with much more efficient
generated code.

In fact, there's a whole host of clever tricks to play during closure
conversion and lambda lifting, especially in lazy languages.

For more on this, check out SPJ's book
http://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/
