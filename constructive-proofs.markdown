---
title: Proofs and Programs
---

In this post I'd like to discuss something near and dear to my
heart, proofs and programs! As it happens, these are two subjects
are tied together in an interesting way that leaves many developers
a set of powerful tools for reasoning about correctness of their programs.

## Math You Can Run

Let's consider Haskell/MLish language that we'll call X.

Now suppose we have some random expression in X, since it's
statically typed, we can make a typing judgement `e : T`.
This term is a "proof" of some sort. It proves
to us that there is some term of type `T`! And of course
our proof is machine checked, if we wrote `e : T` and
`e : Z` then we'd get a type error from our compiler.

Of course most of the time `T` is an incredibly boring thing
to prove. If I told you that I had proven that `int` is witnessable
you'd probably laugh at me. But, what if we could write more interesting
types?

Consider for example


``` haskell
    foo :: (a -> b) -> a -> b
```

Most people will recognize this trivially as `($)` or `id` in Haskell,
but what proof does it represent?

To me it reads, "When given a proof of `a -> b` and a proof of `a`, then
we can provide a proof of `b`.

Notice how `->` read like logical implication (A implies B)? That's
no coincidence, it turns out that `->` is the programmatic version
of implication.

Now reading with this realization means we have proven
"a proof of `A` and a proof that `A` implies `B` implies a proof of `B`".

Sound familiar? That's modus ponus, the most boring of all theorems :)

Alright, let's try to think of some other parallels to logic.

The logical notion of "truth" is just something we can always proof,
so the programmatic truth should be a `T` so that we can trivially find
an `e`  so that `e : T`. Now the simplist type like this is `unit` or `()`!
It's a type with only one occupant, `()`.

What about false? It's a bit trickier because we don't often deal with
types with *no* occupants at all.. but if we created one in Haskell
it would look like

``` haskell
    data False = -- No constructors, otherwise we could prove false (!)
```

If you don't recognize it right away, that's just [`Void`](http://hackage.haskell.org/package/void-0.6/docs/Data-Void.html).


Now let's move on from simple logical values to constructions. The
most common is implication, but we already know that `(->)`. But what
about and and or?

Well `A /\ B` should mean we have a proof for both `A` and `B`, so
in a program, this must mean we have `a : A`, and `b : B` and then
we just bundle our witnesses into one type. That sounds a lot like
`(a, b)`! In fact it is, as we can see by "proving" the following theorem

``` haskell
    -- A /\ B implies A
    proj1 :: (a, b) -> a
    proj1 (a, _) = a

    -- A /\ B implies B
    proj2 :: (a, b) -> b
    proj2 (_, b) = b
    
    -- A and B implies A /\ B
    inj :: a -> b -> (a, b)
    inj a b = (a, b)
```


Now what about or? If we have `A \/ B` then we can either
prove `A` or `B`, so we must have a proof of one, but not the
other. That sounds a lot like `Either`

``` haskell
    -- A \/ B and a proof that A implies C and B implies C implies C
    proj :: Either a b -> (a -> c) -> (b -> c) -> c
    proj (Left  l) f _ = f l
    proj (Right r) _ f = f r

    -- A implies A \/ B
    inl :: a -> Either a b
    inl a = Left a

    -- B implies A \/ B
    inr b = Right b
```

Finally, let's discuss negation. Negation in programs is weird because
we can't say "There is no such term of type `T`". But what we can say
is that "If such a witness exists, then `False`". And with a bit of
head scratching, we can see that

``` haskell
    type Not a = a -> Void
```

is in fact what we want.

There is one more thing that I haven't mentioned up to this point. In all
of these proofs, I'm implicitly inserting "forall A B" and similar before
their statements just like Haskell is. All that this means is that universal
quantification (`forall`) in logic is equivalent to universal quantification
in Haskell. This also means that if we wanted nested universal quantification
we'll need to enable `RankNTypes` in Haskell. 

So to briefly summarize

 Logic               Program
-------------       ------------
 Implication         Functions
 And                 Tuples
 Or                  `Either`s
 True                 Unit
 False               `Void`
 Not                 `-> Void`
 For all             `forall`


So a logical proposition is just a type,
and a proof of such a proposition is an expression
with the desired type.

This has a fancy name, "The Curry-Howard Isomorphism". Actually,
this isomorphism is a lot richer than what I'm describing and talks
of monads, linear/affine type systems, dependent types, and many others.

However, there's one thing gotcha, we don't have classical logic. The
simplist way to illustrate this is with the logical formula `A /\ not A`.

So in our case, we need a function

``` haskell
    excludedMiddle :: Either A (A -> Void)
```

But, there's no such term with this type (ignore bottom here, we'll
chat about that later). What we actually have is called "intuitionist"
logic or constructive logic. Constructive logic is the same as classical
logic, but without double negation or the law of excluded middle (what we
had above).

The intuition (tee-hee) for this is that without those two laws, we need
to "prove" everything directly. Every proposition provides a direct
witness for its result and we *need* a witness to write a program as our
proof.

## Useful Proofs

Now, while what I have might be fun from an intellectual point of
view, it's pretty boring frankly. There's not a whole lot of
"real world theorems" you can create using just `\/` and `/\`.

What we really want is a method of introducing new logical constructions that
describe properties we're interested in. The way to do this is actually
trivial, algebraic data types!

Put simply, the type of our ADT is our proposition. The constructors
are introduction rules and pattern matching is a way of using results.

So, what interesting propositions can we state with vanilla ADTs?
Still not that much unfortunately.

With two simple additions though, we can prove a lot. The first
is adding GADTs, the second getting an actual interesting kind
system.
