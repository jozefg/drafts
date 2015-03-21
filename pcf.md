---
title: A Tiny Compiler For A Typed Higher Order Language
tags: compilers, types, haskell
---

Hi folks, the last week or so I was a little tired of schoolwork so I
decided to scratch out some fun code. The end result is an
[extremely small compiler][pcf] for a typed, higher order functional
language called PCF to C. In this post I'll explain attempt to explain
the whole thing, from front to back :)

## What's PCF

First things first, it's important to define the language we're
compiling. The language, PCF, is an extremely small language you
generally find in a book on programming languages, it originates with
Plotkin if I'm not mistaken.

PCF is based around 3 core elements: natural numbers, functions
(closures), and general recursion. There are two constants for
creating numbers, `Zero` and `Suc`. `Zero` is self explanatory and
`Suc e` is the successor of the natural number `e` evaluates to. In
most programming languages this just means `Suc e = 1 + e` but `+`
isn't a primitive in PCF (we can define it as a normal
function).

For functions, we have lambdas like you'd find in any functional
language. Since PCF includes no polymorphism it's necessary to
annotate the function's argument with it's type.

Finally, the weird bit: recursion. In PCF we write recursive things
with `fix x : τ in e`. Here we get to use `x` in `e` and we should
understand that `x` "stands for" the whole expression, `fix ...`. As
an example, here's how we define `+`.

``` haskell
    plus =
          fix rec : nat -> nat -> nat in
            λ m : nat.
            λ n : nat.
              ifz m {
                  Zero  => n
                | Suc x => Suc (rec x n)
              }
```

## Now Let's Compile It
### Type Checking
### Closure Conversion
### Lambda Hoisting
### Making Allocation Explicit
### Converting To SSA-ish C
### The (Ugly) C Bits
## Wrap Up
[pcf]: http://github.com/jozefg/pcf
