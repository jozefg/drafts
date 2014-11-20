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
can imagine formatting every sane recursive type
