---
title: A Crash Course on ML Modules
tags: sml, haskell
---

I was having lunch with a couple of Haskell programmers the other day
and the subject of the ML family came up. I've been writing a lot of
ML lately and mentioned that I thought SML was well worth learning for
the average Haskeller. When pressed why the best answer I could come
up with was "Well.. clean language, Oh! And an awesome module system"
which wasn't my most compelling response.

I'd like to outline a bit of SML module system here to help
substantiate why looking at SML is a Good Thing. All the code here
should be translatable to OCaml if that's more your taste.

## Concepts

In ML languages modules are a well thought out portion of the
language. They aren't just "Oh we need to separate these
names... modules should work". Like any good language they have
methods for abstraction and composition. Additionally, like any good
part of an ML language, modules have an expressive type language.

So to explain how this module system functions as a whole, we'll cover
3 parts

 1. Structures
 2. Signatures
 3. Functors

## Structures

Structures are the values in the module language. They are how we
actually create a module. They look like this.

``` sml
    struct
      fun flip f x y = f y x
      datatype 'a list = Con ('a * 'a list) | Nil
      ...
    end
```

So they're just a bunch of a declarations stuffed in between a
`struct` and `end`. This is a bit useless if we can't bind it to a
name. For that there's

``` sml
    structure M = struct val x = 1 end
```

And now we have a new module `M` with a single member, `x : int`. This
is just like binding a variable in the term language except a "level
up" if you like. We can use this just like you would use modules in
any other language.

``` sml
    val x' = M.x + 1
```

Since `struct ... end` can contain any list of declarations we can
nest module bindings.

``` sml
    structure M' =
      struct
        structure NestedM = M
      end
```

And access this using `.`.

``` sml
    val sum = M'.NestedM.x + M.x
```

As you can imagine, it would get a bit tedious if we needed to `.` our
way to every single module access. For that we have `open` which just
dumps a module's exposed contents into our namespace. What's
particularly interesting about `open` is that it is a "normal"
declaration and can be nested with `let`.

``` sml
    fun f y =
      let open M in
        x + y
      end
```

OCaml has gone a step further and added special syntax for small
opens. The "local opens" would turn our code into

``` ocaml
    let f y = M.(x + y)
```

This already gives us a lot more power than your average module
system. Structures basically encapsulate what we'd expect in a module
system, but

 1. Structures =/= files
 2. Structures can be bound to names
 3. Structures can be nested

Up next is a look at what sort of type system we can impose on our
language of structures.

## Signatures
