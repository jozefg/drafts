---
title: Examining Hackage: operational
tags: haskell
---

In this installment of "jozefg is confused by other people's code" we
turn to `operational`. This is a package that's a little less known
than I'd like. It provides a monad for transforming an ADT of
instructions a monad that can be used with `do` notation and separates
out interpenetration.

Most people familiar with free monads are wondering what the
difference is between operational's approach and using
[free monads][you-could-have]. In short, I have no clue.


[you-could-have]: http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
