---
title: Some Quick Haddock Tips
tags: haskell
---

I've been writing a [package][folds-common] lately which consists of a
lot of very small function. To make this usable I've been trying to
add quite a lot of documentation for each function and in doing so
have run up against Haddock.

In fact, it seems like most of the time I try to maintain a package I
end up in the Haddock manual wondering why nothing is rendering the
way I'd like it to.

To help remedy this, I'm writing a quick accumulation of Haddock
notes. Now I can reference *this* every time I need to write
documentation. Progress!

## Haddock Elements

Before discussing the specifics of the markup language it's worth
reminding ourselves how Haddock sees the world. Haddock will render
each Haskell module as a page and within that page there are several
distinct elements.

To start with, there's the module header. This is meant to be a TLDR
of sorts for the module.

## Markup Commands
## Random Features
## Wrap Up

To finish, I'd like to mention a few projects that have quite a lot of
pretty documentation. To start with, almost anything with Gabriel
Gonzalez or Edward Kmett as the authors has a plethora of good
documentation. Poking around the source tends to turn up some nice
little pieces of Haddockified comments.

Specifically I'd look at

 - [lens][lens]
 - [pipes][pipes]
 - [folds][folds]

Finally [haskell][github-haskell] is home to a lot of widely used
Haskell code. Chances are high that most of it is reasonably
documented and worth peeking at.

In particular, I'd like to point out pipes' trick of using a
`.Tutorial` module to give a quick tutorial of the module on
Hackage. People who's packages I use: you should steal this, it's
great.

Well dear reader, now that we have even fewer reasons not to document
our code I'm off to write some.

[folds-common]: http://github.com/jozefg/folds-common
[lens]: http://github.com/ekmett/lens
[folds]: http://github.com/ekmett/folds
[pipes]: https://github.com/Gabriel439/Haskell-Pipes-Library
[github-haskell]: https://github.com/haskell
