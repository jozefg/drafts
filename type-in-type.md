---
title: Type is not in Type
tags: jonprl, types, haskell
---

I was reading a [recent proposal][dep-haskell] to merge types and
kinds in Haskell to start the transition to dependently typed
Haskell. One thing that caught my eye as I was reading it was that
this proposal adds `* :: *` to the type system. This is of some
significance because it means that once this is fully realized Haskell
will be inconsistent. Of course, this isn't a huge deal since Haskell
is already woefully inconsistent with

 - `unsafePerformIO`
 - Recursive bindings
 - Recursive types
 - Exceptions
 - ...

So it's not like we'll be entering new territory here. This is an
issue of significance though for languages like Idris or Agda where
such a thing would actually render proofs useless. Famously,
Martin-Lof's original type theory (the 1971 version AFAIK) did have
`Type : Type` (or `* :: *` in Haskell spelling) and Girard managed to
derive a contradiction (Girard's paradox). The particulars of this
construction are a little bit complicated but it's interesting to note
why exactly this goes wrong though. In this post I'd like to prove
that `Type : Type` is a contradiction in [JonPRL][jonprl]. This is a
little interesting because in most proof assistants this would work in
two steps

 1. Hack the compiler to add the rule
 2. Construct a contradiction and check it with the modified compiler

*OK to be fair, in something like Agda you could use the compiler
 hacking they've already done and just say `{-# OPTIONS --set-in-set
 #-}` or whatever the flag is. The spirit of the development is the
 same though*

in JonPRL I'm just going to prove this as a regular implication. We
have a proposition which internalizes membership and I'll demonstrate
`member(U{i}; U{i}) -> void` is provable.

## Background on JonPRL

## The Main Result

## Wrap Up
