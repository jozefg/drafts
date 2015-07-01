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

In particular it's important to distinguish the difference between `∈`
the judgment and ∈ the term. There's nothing inherent in `∈` above
that makes it behave like a typing relation as you might
expect. Later, we're going to construct some rules around `∈` that are
going to make it behave that way but for now, it and `=` are just
suggestively named constants.

This term language contains the full untyped lambda calculus so we can
write all sorts of fun programs like

``` jonprl
    λ(f.ap(λ(x.ap(f;(ap(x;x)))); λ(x.ap(f;(ap(x;x)))))
```

which is just the Y combinator. In particular this means that there's
no reason that every term in this language should normalize to a
value. There are plenty of terms in here that diverge and in
principle, there's nothing that rules out them doing even stranger
things than that. We really only depend on them being deterministic,
that `e ⇒ v` and `e ⇒ v'` implies that `v = v'`.

### Tactics

The other big language in JonPRL is the language of tactics. Luckily,
this is very familiarly territory if you're a Coq user. Unluckily, if
you've never heard of Coq's tactic mechanism this will seem completely
alien. As a quick high level idea for what tactics are:

When we're proving something in a theorem prover we have to deal with
a lot of boring mechanical details. For example, when proving
`A → B → A` I have to describe that I want to introduce the `A` and
the `B` into my context, then I have to suggest using that `A` the
context as a solution to the goal. Bleh. All of that is pretty obvious
so let's just get the computer to do it! In fact, we can build up a
DSL of composable "proof procedures" or tactics to modify a particular
goal we're trying to prove so that we don't have to think so much
about the low level details of the proof being generated. In the end
this DSL will generate a proof term (or verification in JonPRL) and
we'll check that so we never have to trust the actual tactics to be
sound.

In Coq this is used to great effect. In particular see Adam Chlipala's
[book][cpdt] to see incredibly complex theorems with one line proofs
thanks to tactics.

In JonPRL the tactic system is quite simple, to start we have a couple
of basic tactics which are useful no matter what goal you're
attempting to prove

 - `id` a tactic which does nothing
 - `t1; t2` this runs the `t1` tactic and runs `t2` on any resulting
    subgoals
 - `*{t}` this runs `t` as long as `t` does *something* to the
   goal. If `t` ever fails for whatever reason it merely stops
   running, it doesn't fail itself
 - `?{t}` tries to run `t` once. If `t` fails nothing happens
 - `!{t}` runs `t` and if `t` does anything besides complete the proof
   it fails. This means that `!{id}` for example will always fail.
 - `t1 | t2` runs `t1` and if it fails it runs `t2`. Only one of the
   effects for `t1` and `t2` will be shown.
 - `trace "some words"` will print `some words` to standard out. This
   is useful when trying to figure out why things haven't gone your
   way.
 - `fail` is the opposite of `id`, it just fails. This is actually
   quite useful for forcing backtracking and one could probably
   implement a makeshift `!{}` as `t; fail`.

Now those give us a sort of bedrock for building up scripts of
tactics. We also have a bunch of tactics that actually let us
manipulate things we're trying to prove. The 4 big ones to be aware of
are

 - `intro [some term]`
 - `elim #NUM [some term]`
 - `eq-cd`
 - `mem-cd`

## Built in Constructs

## Built in Tactics

## Fun Uses


[jon]: http://www.jonmsterling.com/
[jonprl]: https://github.com/jonsterling/JonPRL
[nj]: http://www.smlnj.org/
[mode]: https://github.com/david-christiansen/jonprl-mode
[cpdt]: http://adam.chlipala.net/cpdt/
