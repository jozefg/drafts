---
title: ASTs and Stuff
tags: compilers
---

After my last post someone commented wanting an explanation of how program is
represented inside a compiler. It turns out that this questions bring up a lot
of subtle points about how best to design a compiler. In this post, we'll go
through some of them.

## What Information Do We Want

Before we can ask ourselves how to represent a program we have to decide what
parts of the program are worth keeping around. We have a few constraints to keep
in mind

 1. Make the representation faithful to the program written

    We want to be able to present some sort of reasonable error message to the
    user. In order to do this we need to have enough information to say "look,
    you screwed up in this leaf of the tree which was this piece of code".

    Not everything has to be preserved, but we need to not drop major syntactic
    constructs if we want
