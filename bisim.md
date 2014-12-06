---
title: Treating Programs like Vending Machines
tags: haskell, types
---

Proving things about programs is quite hard. In order to make it
simpler, we often lie a bit. We do this quite a lot in Haskell when we
say things like "ignoring bottom" or "assuming everything
terminates". Most of the time, this is alright. We sometimes need to
leave the universe of terminating things, though, whenever we want to
prove things about streams or other infinite structures.

In fact, once we step into the wondrous world of the infinite we can't
rely on induction anymore. This is quite a serious issue since
induction was one of our few solid tools for proof. We can replace it
though with a nifty trick called bisimulation which gives rise to
coinduction.

## Labeled Transition Systems

Before we get to proving programs correct, let's start with proving
something simpler. The equivalence of two simple machines. These
machines (A and B) have 3 buttons. Each time we push a button the
machine reconfigures itself. A nice real world example of such
machines would be vending machines. We push a button for coke and out
pops a (very dusty) can of coke and the machine is now slightly
different.

Intuitively, we might say that two vending machines are equivalent if
and only if our interactions with them can't distinguish one from the
other. That is to say, pushing the same buttons on one gives the same
output and leaves both machines in equivalent states.

To formalize this, we first need to formalize our notion of a vending
machine (that was a weird sentence to type).
