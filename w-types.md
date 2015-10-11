---
title: W[eird] Types
tags: types, agda
---

I haven't posted any of the fun Agda code I've been writing lately,
now seems like as good a time as any to change it!

Let's go over a fun little gem: encoding all well founded types as
nonrecursive types fed to one monstrous type: `W`. I'm reasonably
certain this trick dates back to Martin-Lof, but don't quote me.

First let's start by looking at the classic recursive type: `Nat`.
Each `Nat` is either the successor of another or zero. We can
therefore visualize a `Nat` as

    S - S - S - S - Z

A tree of constructor applications which terminates as `Z`. Really, we
can imagine formatting every sane recursive type as a tree. Each
node is a tuple of the non-recursive arguments given to a constructor
and a node's children are the recursive bits of that node's
constructor. For `Nat` there are only recursive arguments so it looks
like a list of `S`'s followed be a `Z`. For an actual list, we'd have

    Cons 1 - Cons 2 - Cons 3 - Nil

And for actual trees we might have

         Node 'a'
            |
          -- --
         |     |
     Node 'b' Leaf
         |
       -- --
      |     |
     Leaf  Leaf

So we have this pattern that seems to capture a lot of useful
inductive types. Can we write this down as it's own inductive
definition?

Well we have `n` types of nodes, annotated with some stuff and
depending on what type of node different numbers of children. If we
represent the types of nodes with

``` agda
    data W (A : Set) (F : A → Set) : Set where
      sup : A → (F a → W A F) → W A F
```

 So `A`s are the particular type of node we're at, you can think of it
as determining what constructor we're using, `F a` is then a sort of
collection of keys. For each distinct `x : F a` we can get a recursive
subterm of the term we're looking at. Each node has the type `a : A`
and `F` determines the shape of the recursive calls given the type of
the node.

For example, for natural numbers we have two types of nodes which we
represent with `bool`. For the successor case we have only one
recursive call. What sort of argument should a function take if it can
only ever return 1 answer? `⊤` of coures! When we're in the `Z` case
there are no recursive calls so `F false` must be uninhabitable. This
means

``` agda
    NatF : Bool → Set
    NatF true = ⊤
    NatF false = ⊥
```

We can then define

``` agda
    Nat : Set
    Nat = W Bool NatF
```

To see that this is more or less equivalent to `ℕ` as we're used to
it, look at this

``` agda
    left : ℕ → Nat
    left zero = sup false (λ ())
    left (suc n) = sup true (λ _ → left n)
```

So you can see that each `ℕ` can be converted by to a `Nat`. We change
each constructor to `sup` + some particular `Bool`. From there we use
just recurse to construct a term of type `NatF b → Nat`. Going the
other way is similar, except instead of constructing that lambda we'll
apply it!

``` agda
    right : Nat → ℕ
    right (sup false _) = zero
    right (sup true f) = suc (right (f tt))
```

In the first case, the function has the type `⊥ → Nat` which is pretty
useless so we just ignore it and return `zero`. In the next case, `f :
⊤ → Nat` so we can apply it to `tt`, `f tt : Nat`. The intuition being
that this is `n` and we're looking at `suc n` in it's W-tree
presentation.

Proving that `left` and `right` are inverses isn't so simple. The
issue is that `left ∘ right` is going to involve building new lambdas
by applying the old functions, ie will end up with `sup false (λ ())`
instead of `sup false f`! Now we know these are extensionally
equivalent since any two functions from `⊥` are, but extensional
equivalence doesn't apply ≡ in Agda! At best we can just assert
`funext` and live with it.

``` agda
    postulate funext : {A B : Set}(f g : A → B) → ((a : A) → f a ≡ g a) → f ≡ g

    idR : (n : Nat) → left (right n) ≡ n
    idR (sup true x) rewrite idR (x tt) | funext _ _ (λ _ → refl) = refl
    idR (sup false x) rewrite funext x ⊥-elim (λ b → ⊥-elim b) = refl
```

The proof proceeds by induction and we use `funext` to show that the
functions we get back are equal to the ones we put in. The other
direction is easier and we don't need `funext` for it.

So now we have our dilemma, W-types work great for modeling these
inductive types, but we can't prove them to be equal directly because
our notion of equality forbids us from identifying the functions we
generate with the functions we start with when manipulating
w-types. For example, we can package up the recursion principle (the
non-dependent thing) for a W-type into this function

``` agda
    cata : {A B : Set}{F : A → Set} → W F → ((a : A) → (F a → B) → B) → B
    cata (sup a x) f = f a (λ z → cata (x z) f)
```

One thing we'd like to be able to do is prove that `cata` has the
properties we'd expect a catamorphism to have. `W` after all can be
thought as an initial F-algebra with the appropriate categorical
semantics. In particular, since `W F` is supposed to be initial, it
should be the case that there is a map from unique map `W F → W F`,
since `λ x → cata x sup : W F → W F` we can therefore infer that `cata
_ sup` should just be `id`. Proving this holds those is a bit
troublesome.

``` agda
    cata-id : {A : Set}{F : A → Set}(w : W F) → cata w sup ≡ w
    cata-id (sup a x) = {!!}
```

gives us the goal

    Goal: sup a (λ z → cata (x z) sup) ≡ sup a x
    ————————————————————————————————————————————————————————————
    x  : .F a → W .F
    a  : .A
    .F : .A → Set
    .A : Set

Here's the problem though, we'd like to use our IH to rewrite `cata (x
z) sup` to `x z` at which point we have our goal up to
eta-expansion. We can't perform this rewrite though! In order to do so
we need to be able to say "If for all `x` `f x ≡ g x` then `f ≡ g`"
which is just functional extensionality. Without our `funext`
postulate, we're completely stuck just trying to get out of the gate.

This means that while W-types are nice in theory, in practice in an
intensional type theory there's a fair amount of pain in using
them. This doesn't mean that W-types are useless, just that they're
useless in this setting.

## In Computational Type Theory

Way in 1979ish, Martin-Lof proposed this type theory here termed
"Intuitionistic Type Theory" (though everyone else just calls it
Martin-Lof Type Theory, MLTT). Time passed and eventually this flavor
of type theory was split off into the type theory that Agda and Coq
are based on and the type theory that NuPRL is based on, computational
type theory.

Computational type theory in particular is less concerned with this
whole "decidable equality" thing and so it admits functional
extensionality right off the bat. We can even show that W-types are
sufficient to encode all the types

It states that if we have a proof that `I(A, a, b)` (written `a ≡ b`)
then we can derive that the *judgment* `a = b ∈ A` holds. This is
actually a very interesting.
