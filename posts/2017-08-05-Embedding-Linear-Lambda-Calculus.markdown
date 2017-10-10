---
title: Embedding Linear Lambda Calculus, Quickly and Easily
author: Phil Freeman
date: 2017/08/05
description:
tags: PureScript
---

Suppose you want to create an embedded DSL based on the linear lambda calculus. Why might you want to do this? Well, you might want to control access to some resource, or perhaps you've heard that [linear types can change the world][Wadler] and now you'd like to compile your EDSL to some target language and optimize things using mutable data structures.

There are existing techniques for embedding linear lambda calculus in a typed language like Haskell. For example, [one approach is to use higher order abstract syntax][Polakow]. However, I'm going to show a simpler (but less powerful) technique. We won't get multiplicatives and additives, and we won't get a separate non-linear function type, but the implementation is tiny.

The [`purescript-substructural`][Substructural] library by @rightfold uses this technique to embed functions which are allowed to modify mutable data structures in place, and provides a great example with readable source code.

[Wadler]: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.31.5002 "Liner types can change the world!"
[Polakow]: http://dl.acm.org/citation.cfm?id=2804309 "Embedding a full linear lambda calculus in Haskell"
[Substructural]: https://pursuit.purescript.org/packages/purescript-substructural/ "`purescript-substructural`"

## Abstraction Elimination

Abstraction elimination is the process of turning regular lambda calculus terms into point-free combinators. Proof-theoretically, it corresponds to the deduction theorem from first-order logic.

For the regular non-linear lambda calculus, abstraction elimination results in a term formed using the SKI basis. However, this won't work for the linear lambda calculus, since the S and K combinators are not linear! Instead, we can generate a term using the ["BCI" system][BCKW].

The `B` combinator is regular function composition:

```
B = \x y z -> x (y z)
```

And the `C` combinator is the `flip` function:

```
C = \x y z -> (x z) y
```

Now, consider the name `z` in each of these (linear) terms. It can only be used once, and in the case of the `B` combinator, it is used on the right of the outer function application, and on the left in the case of the `C` combinator.

This gives us an idea of how abstraction elimination can work in a linear setting - each time we see a function application, the variable we are trying to eliminate can only appear on one side of the application. Without loss of generality, it appears on the right (apply `C` if necessary), so we can eliminate the variable recursively using `B`.

Note that unlike regular abstraction elimination to SKI term, this algorithm has two benefits:

- There is no exponential explosion in term size, since we only need to recurse into one side of each function application.
- The `B` and `C` combinators correspond to functions we use every day: `compose` and `flip`, so abstraction elimination actually results in understandable terms.

[BCKW]: https://en.wikipedia.org/wiki/B,_C,_K,_W_system "B, C, K, W system"

## Example

As an example, consider the following lambda calculus term:

```
\a b c d e -> a (b c) (d e)
```

Let's eliminate `e` first. It appears on the right, so we can use `B`:

```
\a b c d -> B (a (b c)) d
```

Eliminating `d` is easy, using eta conversion:

```
\a b c -> B (a (b c))
```

Next, we can use `B` twice to eliminate `c`:

```
\a b c -> B ((B a b) c)
= \a b -> B B (B a b)
```

To eliminate `b`, again notice that it appears on the right:

```
\a b -> (B B) ((B a) b)
= \a -> B (B B) (B a)
```

Finally, one more application of `B` eliminates `a`, giving our final point-free term:

```
B (B (B B)) B
```

## A note about ordered, affine and relevant terms

Notice, we never actually used the `C` combinator here. The reason is that nothing needed to be flipped, because all names were used in order.

The _ordered_ linear lambda calculus consists of those terms which satisfy this property - not only must a name be used exactly once, but it must be used in the order it was declared.

In fact, it's possible to express any ordered linear lambda calculus term using only the `B` and `I` combinators. I've written up an implementation of the algorithm in PureScript [here](http://try.purescript.org/?gist=7c3055b578a58428aeb31cfa43162b23).

Notice that ordered linear terms can be interpreted in any category, since `B` and `I` are just the `compose` and `identity` functions!

Instead of dropping the `C` combinator, we could instead _add_ the `K` combinator. The `K` combinator corresponds to the `const` function, and ignores its argument. We get _affine_ lambda calculus terms, in which every function argument must be used at most once.

We can also add the `W` combinator if we want _relevant_ lambda calculus, in which arguments must be used at least once.

If we add both `K` and `W`, we get regular old lambda calculus again, via the BCKW basis.

## Embedding linear lambda calculus

So, now that we know how to perform abstraction elimination, how can we use this to embed a language of linear lambda calculus terms?

Well, simply create an abstract data type for linear terms, and hide the constructor. Provide a `Category` instance (which implements the `B` and `I` combinators) and a `flip` function (which implements the `C` combinator). In order to make your DSL useful, you will also want to provide some standard terms which the user can combine.

This approach requires the user to perform abstraction elimination on terms, but in practice, this is not a big problem. As we've seen, the generated terms can be quite small, and straightforward. With something like typed holes (or even _type-directed search_), it can be quite simple to build these terms directly.

Because the constructor is hidden, we know that every term the user builds must use its argument linearly, and we might be able to exploit this property for optimization purposes

As another little example, [this](http://try.purescript.org/?gist=51d7971a85d6e1cd38b02e04e2fa1ced) shows how we might use this approach to ensure that resources such as file handles are always accessed exactly once.
