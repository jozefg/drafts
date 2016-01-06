---
title: Metric Spaces for Semantics
tags: types
---

In this post I'd like to explain a neat idea that turns out to have
many interesting applications: modeling programs as points in a
metric space. We'll walk through semantics for PCF based on metric
spaces. First, we need to start with what PCF is:

## PCF

PCF is short for "partial computable functions". It's a very simple
but turing complete language. In [previous posts][prev] we've worked
through a compiler for PCF. In short, PCF has 3 key concepts,

 - Functions: this gives us lambdas (`λ x. e`) and applications (`e
 e`). These are given the type `τ₁ → τ₂`.
 - Natural numbers: for natural numbers we have the operations `zero`
   and `succ(-)`, these let us write down the natural numbers (albeit
   not concisely). We'd write 3 as `succ(succ(succ(zero)))`. In order
   to deal with numbers we use `ifz`, it's a simple branching
   construct that just lets us case on whether a number is zero or the
   successor something, `ifz(succ(zero); succ(zero); x.x)`. Read this
   as `ifz(THING WE'RE MATCHING; ZERO CASE; previous. SUCCESSOR CASE)`.
 - Finally, Recursion: We have the ability to write expressions which
   refer to themselves in PCF. This gives us most of the power of
   PCF. The syntax is `fix(x. e)`. In this, `x` may be used in `e` and
   stands for the entire `fix(x.e)` expression. If `e = λ a. ...` then
   this means that we can write recursive functions.

As a quick illustration, here's how we'd write factorial

    factorial ≙ fix(rec. λ n.
                    ifz(n;
                        succ(zero);
                        prev. n * rec prev))

Assuming that we've correctly defined `*` which is not hard (fun
challenge, write `+` and `*` in your favorite functional language
limiting yourself to constructs like `fix` and `ifz`).

Now PCF is simple, but interesting enough to give rise to some fun
models. The nontermination aspect means that there's an elegant model
in domains, there what we'd turn `e : τ` into some element of a domain
`Dₜ` and make use of the fact that any continuous function `Dₜ → Dₜ`
has a fixed point if `Dₜ` is pointed. This lets us express
`fix`. However, there's an equally interesting model where we replace
`Dₜ` with `Mₜ`, an appropriate complete metric space and "continuous
function `Dₜ → Dₜ`" with "continuous contractive function `Mₜ →
Mₜ`". First, a bit of background on (complete) metric spaces.

## Complete Metric Spaces

Metric spaces are quite simple in their definition, they're just a
simple abstraction on top of sets with a notion of distance. A metric
space is a pair `(X, d)` where `d : X × X → R` so that

 - `d(x, y) = 0` if and only if `x = y`
 - `d(x, y) ≥ 0` for all `x`, `y`
 - `d(x, y) = d(y, x)`
 - `d(x, z) ≤ d(x, y) + d(y, z)` for all `x`, `y`, `z`

If you look at these conditions they all mostly make sense if you
consider `X = R` and `d(x, y) = |x - y|` (eg d is just a simple
measure of distance). The last condition has a name by the way, it's
the triangle inequality.

Now metric spaces have enough information to generalize familiar
notions from calculus (or just analysis). Specifically, suppose we
have some function `f : X → Y` where `X` and `Y` are metric spaces, we
can generalize the ε-δ definition of convergence we would use if `X =
Y = R`. That is, `f` is continuous if and only if for all `ε > 0`,
there exists a `δ > 0` so that if `d(x, y) < δ` then
`d(f(x), f(y)) < ε`. The intuition for this is that a continuous
function is one so that if we restrict how much the input differs
then the output can only change a little bit. This implies that a
continuous function is free of big "jumps".

Another important notion is that of a Cauchy sequence. This is an
infinite sequence of elements in some metric space `X`, call it `x₁`,
`x₂`, `xₙ`, so that as you go further along in the sequence the
elements get closer and closer together. Arbitrarily so. Phrased
formally, we say that a sequence `(xₙ)ₙ` is a Cauchy sequence if given
an `ε`, there exists an `N` so that if `n, m > N` then `d(xₙ, dₘ) < ε`.
A Cauchy sequence is said to converge if all the points in the
sequence group closer and closer to any one particular point. Not all
sequence need to converge. For example, consider the sequence `xₙ =
exp(-n)`. This converges to `0` in `R` with the usual metric of
distance, but in `R - {0}` (same metric) this sequence is still well
defined as 0 isn't a term but cannot converge to anything because
we've deleted what it converged to from the space! We formalize
convergence to a point just by saying `(xₙ)ₙ` converges to `x` if for
all `ε > 0` there is an `N > 0` so that if `n > N` then
`d(xₙ, x) < ε`. We say a metric space is "complete" if every Cauchy
sequence converges to some point in the space. Famously, the real
numbers are a complete metric space but the rational numbers certainly
are not (`R` is the extension of `Q` that adds enough points to make
all the Cauchy sequences converge called a completion).

One last definition and then a nifty theorem: a continuous function `f
: X → Y` is said to be "contractive" if for any `x, y` so that
`d(f(x), f(y)) = c d(x, y)` for some fixed `0 < c < 1`. Now I claim
that any contractive `f : X → X` has a fixed point if `X` is a
nonempty complete metric space. To see this, construct the sequence
`xₙ = fⁿ(x)` where `x₀ = a` for any random `a ∈ X`. I claim this is a
Cauchy sequence, to see why say that `r = d(x, f(x))`. Then
`d(x₀, x₁) = r` simply by definition. Furthermore,
`d(f(x), f(f(x))) = c d(x, f(x))` for some appropriate
`c < 1`. Generalizing a bit with simple induction, `d(xₙ, xₘ)` where
`m = n + 1` is `cⁿr`. By the triangle inequality, we can conclude that
`d(xₙ, xₘ) ≤ cⁿr + c * cⁿ r + ... + cᵐ r`. But this is just a simple
geometric series, it sums to `cⁿr (1 - cᵐ/cⁿ)/(1 - c)`. This is less
then `cⁿr/(1 - c)` by just letting the distance between `n` and `m`
grow arbitrarily. In order to show that this is a Cauchy series, we
note that if we have some `ε > 0` we can choose an `N` so that
`cᴺr/(1 - c) < ε` and thus `d(xₙ, xₘ) < ε` for any `m, n > N`. Thus
this is a Cauchy series. Moreover, since we have a complete metric
space, we know that this series converges to some `x`. Furthermore, if we let `yₙ
= f(xₙ)` we can prove that it is a Cauchy series and converges to the
same `x`. However, since continuous functions preserve limits of
Cauchy series (not hard to prove) this means that `f(x) = x` as
required.

This new fixed point lemma gives us the ability to model the recursion
of PCF. We now have everything we need to construct the model, we're
going to build a complete metric space at each type and prove that if
`〚e〛(ρ) ∈ X` is the denotation of some program `e` with free
variables being given in `ρ`, then `e` forms a contractive map from
the denotations of the values of its free variables to `X`.

## The Metric Spaces

First we define the metric space we'll use to model our programs. We
construct a family of metric spaces `Mₜ` by induction on `t`. For this
we first need to define a metric space `Mₙ`, this will be used to
model numbers. The set of `Mₙ` is `N × N ∪ {∞}` surprisingly. We don't
want to just use `N` as you might expect because there's no notion of
distance on it that's going to let us take fixed points as we hinted
at above. Furthermore, we tack on this extra `∞` which we'll use to
model nontermination, opting for the nonstandard ∞ instead of ⊥ will
make more sense in a moment. Instead we add an extra natural number
whose sole purpose is to give us a distance measurement. This means
our metric is a little strange

    d(∞, ∞) = 0
    d(∞, (m₁, m₂)) = 1/2ᵐ
    d((n₁, n₂), ∞) = 1/2ⁿ
    d((n, x), (n, x)) = 0
    d((n₁, n₂), (m₁, m₂)) = 1/2ᵏ where k = min(n₁, m₁)


There's a lot of weird clauses here, but let me give some intuition:
the second number is the "data". The first point is the amount of work
we will have to do to retrieve that data. Points in this metric space
should be thought of representations of expressions computing natural
numbers. They've dropped all the information about *how* they computed
the numbers, but they retain information on what work they did to get
them. This distance metric could be thought of how longn we have to
wait to tell whether or not to computations are the same or not, the
closer together the longer it takes. We also tack on ∞ to represent
computations which diverge. Now we have a few corner cases to make
sure that they same computations are at distance zero, but the general
idea is that we can't really make observations about a program which
is running, just that it *is* running. However, if we're comparing to
idealized programs and one halts before another than we immediately
can conclude that they're not equal because they ran in different
times. In our distance metric, if two programs are the same no amount
of waiting will tell them apart, if they're different the amount of
waiting is determined by how long it takes the quickest program to
terminate (in the case that one of them diverges it's the other, if
neither diverges it the one with the smallest first component).

Now this gives us a metric space, to see that it's complete note that
every of the form `(n₁, n₂)` is at least `1/2ⁿ` from its closest
neighborhood (we can tell this computation apart if we wait `n₁`
seconds). If we have some Cauchy sequence, for any `x` we can find an
`N` so that all the `xₙ` are with `1/2ˣ` after `N`. This means one of
two things, either one `xₙ` where `n > N` is of the form `(m₁, m₂)`
where `m₁ ≤ x` or it isn't. If it is, then all `xₙ = xₘ` where
`n, m > N`. This is because if `xₙ ≠ xₘ` then
`d(xₙ, xₘ) ≥ 1/2ᵐ > 1/2ˣ` which contradicts our assumption that we
have a Cauchy sequence. If this is not the case, then for any `xₙ`,
it's either ∞ or `(m₁, m₂)` where `m₁ > x`. If we ever are in the
first case for any `x`, then our Cauchy sequence trivial converges
because all the points after a certain initial sequence are the same,
we just pick that as what we're converging to! If the latter case
holds for every `x` then our Cauchy sequence converges to ∞ because as
we go further along in our sequence, each computation takes steadily
longer and longer, arbitrarily so. This means that no matter how long
we wait (`x`), I can pick an `N` so that for all `n > N` it takes `xₙ`
longer than `x` amount of time to halt so `d(xₙ, ∞) < 1/2ˣ`. This is
precisely the definition of convergence though. Thus this is a
complete space.

We now just need to define what to do with the metric space for `τ₁ →
τ₂` assuming that we already have determined spaces for `τ₁` and
`τ₂`. For this we just take our space to be that of nonexpansive maps
(a contractive map where the distance ratio may be 1 and not strictly
less than it) from `M(τ₁) → M(τ₂)`. The distance measure here is
called the "sup-metric" and is given by the following

     d(f, g) = max {d(f(x), g(x)) | x ∈ M(τ₁)}

This is only well defined because of something I'm sweeping under the
rug: all of these spaces are 1-bounded so that all points are of
distance 1 or less. Anyways, all we need to is show that this is
complete and we're all set. Suppose we have a Cauchy sequence of
functions, `f₁`, `f₂`, .... I claim that they converge to `f`, where
`f(x) = lim fₙ(x)`. Remember that `M(τ₂)` is complete and the distance
metric means that `(fₙ(x))ₙ` is certainly a Cauchy sequence for any
`x`. Proving that `f` gets arbitrarily close to `(fₙ)ₙ` is just an
exercise in εs that I'll skip here. With all of this in hand, we're
finally ready to list off our semantics.


Before we do that though, I'd like to make a quick aside. `Mₙ`
satisfies one important additional property, it's ultrametric. That
is, `d(x, z) ≤ max(d(x, y), d(y, z))`. The intuition for this is as
follows. Let's suppose that `x` halts before `z`, then `d(x, y)` is
determined either by `x` or `y`. If it's determined by `x` (`x` halts
sooner than `y`) then `d(x, z) = d(x, y)` so of course `d(x, z) ≤
max(...)` because it's below one of the arguments to `max`. If `y`
halts first then `d(x, z) < d(x, y)` so the same argument holds. If
`z` halts first a symmetric argument applies. If `x = z` then `d(x, z)
= 0` and so `d(x, z) ≤ d(a, b)` for any two damn points because `d` is
always positive. Ultrametric spaces are important for one particular
reason, they form a cartesian closed category which the function space
being given by the sup metric (more on this in a moment). This means
that the collection of metric spaces we're defining all belong to the
same category (1-bounded ultrametric spaces with nonexpansive maps as
morphisms). It's handy for some other semantic developments to have
this around so I wanted to toss this out as a quick note.
