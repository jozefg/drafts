Hi,

I spend a lot of time thinking about proofs, dependent types, and all
sorts of other fun, mathy things. Now formal verification has become a
pair of taboo words in our community and I don't like it.

Now there are many good reasons why formal verification isn't quite
ready for prime time yet

 1. The tools we have are very very new and not ready for general
    consumption
 2. We're still discovering new abstractions and logics to simplify
    this process
 3. Describing the precise behavior of some systems is *hard*.
 4. Formal verification isn't needed everywhere

I do hear one excuse, however, that is completely false

> Specifications are just programs all over again and we could save
> time by just debugging our original code instead of
> writing/maintaining a specification.

Now I find this to be simply untrue. For example, let's say we write a
compiler for a language. 
