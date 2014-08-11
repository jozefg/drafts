---
title: What Are Dependent Types
---

It seems like dependent types are becoming "popular". Well, as close
to popular as something so staunchly academic gets.. In any case, 
I'd rather like to discuss exactly *what* these dependent types are.

Since I write quite a lot of Haskell, this post is mostly aimed at a
Haskeller interested in some more fancy type systems.

## Quick Note on Notation

Since I'm aiming this post at Haskellers, it's worth taking a moment
to clarify some of the notation I'll use in this post.

The most familiar is `a : T`, which should be read as "`a` has the
type `T`".

Sometimes, we which to indicate that `a` has the type `T` in some
particular scope or environment. An environment is just a list of type
assertions, `a : T, b : U, c : V`. This environment is traditionally
called Γ. So to say that `a : T` in some environment `Γ`, we'd write

    Γ ⊢ a : T

Last but not least, sometimes these type assertions have
preconditions. For example, `f x : B` when `f : A → B` and `x : A`.

We'd write this as

    Γ ⊢ f : A → B,  Γ ⊢ x : A
    —————————————————————————–
       Γ ⊢ f x : B

That should cover it as var as notation goes. If something is
unfamiliar to you, please comment so I can add it to this section.

Now, let's get back to the subject at hand.

## What is a Dependent Type

So the most obvious question to ask is what is a dependent type? Then
unhelpful answer is that a dependent type is one which "depends" on
a value.

This is best explained through example. Let's first talk about
functions. Now the basic type of a function is just

    A → B

and these are governed by a rule something like

    Γ ⊢ x : A, Γ ⊢ f : A → B
    ——————————————————————————
         Γ ⊢ f x : B

now if we allowed our function types to be dependent, then the
result *type* could depend on the input *value*. This is a bit of a
departure from the norm. A function's result value always depends
on the input, but normally the type is fixed, static.

Let's notate the dependent function type like this

   Π (x : A) B

where `B` is some expression that may refer to the free variable
`x`. Now the typing rule for function types becomes

    Γ ⊢ x : A, Γ ⊢ f : Π (x : A) B
    ———————————————————————————————–
         Γ ⊢ f x : B x

Notice the symmetry here, `B` is applied to the argument just like the
actual function value!

Coming from Haskell, this seems kinda useless. After all, what could
`B` possibly be to "use" `x`. 
