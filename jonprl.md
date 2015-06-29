---
title: A Basic Tutorial JonPRL
tags: jonprl, types
---

I was just over at OPLSS for the last two weeks. While there I finally
met [Jon Stering][jon] in person. What was particularly fun is that
for that last few months he's been creating a theorem prover called
[JonPRL][jonprl] in the spirit of NuPRL.

As it turns out, it's quite a fun project to work on so I've
implemented a few features in it over the last couple of days and
learned more or less how it works.

Since there's basically no documentation on it besides the readme and
of course the compiler I thought I'd write down some of the stuff I've
learned.

## Getting JonPRL

JonPRL is pretty easy to build and install. You'll need `smlnj` since
JonPRL is currently written in SML. This is available in most package
managers (including homebrew I've heard?) otherwise just grab the
binary from the [website][nj]. After this the following commands
should get you a working executable

 - `git clone ssh://git@github.com/jonsterling/jonprl`
 - `cd jonprl`
 - `git submodule init`
 - `git submodule update`
 - `make`
 - `make test` (If you're doubtful)

You should now have an executable called `jonprl` in the `bin`
folder. There's no prelude for jonprl so that's it. You can now just
feed it files like any reasonable compiler and watch it spew
(currently meaningless) output at you.

If you're interested in actually writing JonPRL code, you should
probably install David Christiansen's [mode][mode]. Now that we're up
and running, let's actually figure out how the language works

## The Different Languages in JonPRL

JonPRL is composed of really 3 different sorts of constructs

 - The term language
 - The tactic language
 - The language of commands to the theorem prover

In Coq, these roughly correspond to Gallina, Ltac, and Vernacular
respectively.

### The Term Language

The term language is an untyped language that contains a number of
constructs that should be familiar to people who have been exposed to
dependent types before. The actual concrete syntax is composed of 3
basic constructs. We can apply an "operator" (I'll clarify this in a
moment) with `op(arg1; arg2; arg3)`, we have variables with `x`, and
we have abstraction with `x.e`.

An operator in this context is really anything you can imagine having
a node in an AST for a language. So something like λ is an operator,
as is `if` or `pair` (corresponding to `(,)` in Haskell). Each
operator has a piece of information associated with it, called its
arity. This arity tells you how many arguments an operator takes and
how many variables `x.y.z. ...` each is allowed to bind. For example,
with λ has an arity is written `(1)` since it takes 1 argument which
binds 1 variable. Application (`ap`) has the arity `(0; 0)`. It takes
2 arguments neither of which bind a variable.

So as mentioned we have functions and application. This means we could
write `(λ x → x) y` in JonPRL as `ap(λ(x.x); y)`. The type of
functions is written with `Π`. Remember that JonPRL's language has a
notion of dependence so the arity is `(0; 1)`. The construct `Π(A;
x.B)` corresponds to `(x : A) → B` in Agda or `forall (x : A), B` in
Coq.

We also have dependent sums as well (`Σ`s). In Agda you would write
`(A , B)` to introduce a pair and `Σ C λ x → D` to type it. In JonPRL
you have `pair(A; B)` and `Σ(C; x.D)`. To use a `Σ` we have spread
which let's us provide a term which depends on the two components of a
pair. Eg `spread(0; 2)`, you give it a `Σ` in the first spot and
`x.y.e` in the second and it'll replace `x` with the first component
and `y` with the second. Can you think of how to write `fst` and `snd`
with this?

There's sums, so `inl(A)`, `inr(B)` and `sum(C; D)` corresponds to
`Left`, `Right`, and `Either` in Haskell. For case analysis there's
`decide` which has the arity `(0; 1; 1)`. You should read `decide(E;
x.L; y.R)` as something like

``` haskell
    case E of
      Left x -> L
      Right y -> R
```

In addition we have `unit` and `<>` (pronounced axe for axiom
usually). Neither of these takes any arguments so we write them just
as I have above. They correspond to `()` and `()` in Haskell. Finally
there's `void` which is sometimes called bottom or ⊥ in theorem prover
land.

You'll notice that I presented a bunch of types as if they were normal
terms in this section. That's because in this untyped computation
systems types *are literally* just terms. There's no typing relation
to distinguish them yet so they just float around exactly as if they
were λ or something! I call them types because I'm thinking of later
when we have a typing relation built on top of this system but for now
there are really just terms. Nothing more, nothing less. This
justifies introducing several more exotic terms into our language

 - `U{i}`, the ith level universe. This should be thought of as the
   type of types but we don't have types yet and it's not really the
   type of all types. All metaphors suck eventually *shrug*.
 - `=(0; 0; 0)` this is equality between two terms at a type. It's a
   proposition that's going to precisely mirror what's going on later
   in the type theory with the equality judgment
 - `∈(0; 0)` this is just like `=` but behaves as the propositional
   counterpart to typing. It reflects whether a term may be thought of
   as part of a type.

## Built in Constructs

## Built in Tactics

## Fun Uses


[jon]: http://www.jonmsterling.com/
[jonprl]: https://github.com/jonsterling/JonPRL
[nj]: http://www.smlnj.org/
[mode]: https://github.com/david-christiansen/jonprl-mode
