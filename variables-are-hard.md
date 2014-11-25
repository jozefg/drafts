---
title: Slicing λΠ 5 ways
tags: haskell, compilers
———

After my last post, I didn't quite feel like ending there. I was a
little dissatisfied with how binding was handled in the type checker,
the odd blend of HOAS, GUIDs, and DeBruijn variables was... unique.

In the post I explore 5 versions of the same code

 0. The original method
 1. Using `bound` to handle all binding
 2. Using `unbound` to handle all binding
 3. Giving up and just using explicit names
 4. Full HOAS

Fair warning, I've never used `unbound` before and I'm probably using
`bound` in an incredibly backwards way. You've been warned.

## The Original

## `bound`

## `unbound`

## Screw It How Bad Can Real Names Be

## HOAS

## Wrap Up
