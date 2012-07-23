---
title: Abstraction Elimination in Purity
author: Phil Freeman
date: 2012/05/01
description: An explanation of abstraction elimination in the Purity programming language.
tags: Compilers
---

I\'d like to write a little bit about the process of abstraction elimation in the Purity language. Abstraction elimination is the process by which lambda expressions get transformed into point-free code.

This is beneficial to the programmer because programming in a point-free style is not always practical. In a lot of cases, it is much more intuitive to think in terms of variables and closures.

Abstraction elimination is useful from the compiler\'s perspective for the opposite reason: we don't have to worry about things like closures and variables when generating executable code for an expression: we only have to concern ourselves with functions and function composition.

Consider a simple example using lambda expressions - the following snippet defines the S combinator:

> S = \\x y z -> x z (y z)

It is not at all obvious at first how one would write this in a point-free way. In fact, by means of some pretty-printing methods it\'s possible to see what the compiler desugars this into:

> curry (curry ((uncurry (curry ((uncurry (curry ((uncurry (uncurry (curry ((curry ((uncurry (uncurry (curry ((uncurry (curry ((uncurry (uncurry (curry ((id . outl)))) . ((outl, (outl . outr)), (outr . outr))))) . (outl, uncurry (const (id, id))))))) . ((outl, (outl . outr)), (outr . outr)))) . outl)))) . ((outl, (outl . outr)), (outr . outr))))) . (outl, uncurry (const (outl, uncurry (curry ((uncurry (const id), uncurry (curry ((uncurry (curry ((uncurry (uncurry (curry ((id . outl)))) . ((outl, (outl . outr)), (outr . outr))))) . (outl, uncurry (const (id, id))))))))))))))) . ((outl . outl), ((outr . outl), outr)))))

Not pretty at all. One can verify by pasting the above expression into the REPL that it is indeed a valid expression (although it might take a while to compile!)

Obviously, the expression above is far from optimal. The point though is that lambda expressions and abstraction elimination are essential if we want the programmer to be able to express a term like S concisely.

Now let\'s take a look at the abstraction elimination algorithm used in the compiler.

The basic idea is that we visit the lambda expressions in a typed expression tree in a bottom-up fashion. When we reach a lambda expression `v => b`, we have a variable `v`, a variable type `V`, a lambda body `b` and a lambda body type `B`. We want to find a (pointfree) typed expression `p` of type `V -> B` and to substitute this new expression in place of the lambda abstraction in the expression tree. We proceed by case analysis on the type of the body expression. Fortunately, just following the correct types will get us most of the way there.

## Case 1 - The body expression does not depend on the variable v

In this case, we can take `p = const b`.

This is a canonical choice in the presence of the above type constraints. This case also covers constant expressions such as `id`, `inl`, `inr`, `outl`, and `outr`.

## Case 2 - b equals v

In this case, we can take `p = id`. Again, we are directed by the type constraints above, but this makes sense, because in this case the lambda expression in question is just `v => v`.

## Case 3 - b is an application

Suppose `b = f x` and that `x` has type `T` so that `f` has type `T -> B`.

First, we eliminate the variable from the subexpressions `f` and `x` to get expressions `[f]` and `[x]` with types `V -> T -> B` and `V -> T` respectively. We need to construct an expression `[f x]` of type `V -> B`.

We can uncurry `[f]` to get an expression of type `V . T -> B`. Composing with `[x]` in the appropriate component of the product then gives an expression of the correct type:

> [f x] = uncurry [f] . (id, [x])

## Case 3 - b is a composition

Suppose `b = g . f` and that `f` and `g` have types `S -> T` and `T -> U` repectively.

First, we eliminate the variable from the subexpressions `f` and `g` to get expressions `[f]` and `[g]` with types `V -> S -> T` and `V -> T -> U` respectively. We need to construct an expression `[g . f]` of type `V -> S -> U`.

We can uncurry `[f]` and `[g]` to get expressions of type `V . S -> T` and `V . T -> U`.

These are cokleisli arrows in the product comonad and can be composed to give an arrow of type `V . S -> U`. Currying then gives the expression we need:

> [g . f] = curry ((uncurry [f]) . (outl, uncurry [g]))

## Case 4 - b is a constant function

Suppose `b = const x` and that `x` has type `T` so that `b` has type `B = T1 -> T`.

First, we eliminate the variable from the subexpression `x` to get expressions `[x]` with types `V -> T`. We need to construct an expression `[const x]` of type `V -> T1 -> T`.

Composing `[x]` with `outl` gives an expression of type `V . T1 -> T`. Currying now gives the expression we need:

> [const x] = curry ([x] . outl) 

## Case 5 - b is a curried expression

Suppose `b = curry f` and that `f` has type `S . T -> U` so that `b` has type `B = S -> T -> U`.

First, we eliminate the variable from the subexpression `f` to get an expression `[f]` with type `V -> S . T -> U`. We need to construct an expression `[curry f]` of type `V -> S -> T -> U`.

Uncurrying `[f]` gives an expression of type `V . (S . T) -> U`.

At this point we need an auxilliary function which switches the order in which nested products are constructed:

> assocr : (V . S) . T -> V . (S . T)

Composing `uncurry [f]` with `assocr` and currying twice now gives the correct answer:

> [curry f] = curry curry (uncurry [f] . assocr)

## Case 6 - b is a function into a product

Suppose `b = (f, g)` and that `f` and `g` have types `S -> T` and `S -> U` respectively so that `b` has type `B = S ->  T . U`.

First, we eliminate the variable from the subexpression `f` and `g` to get expressions `[f]` and `[g]` with types `V -> S -> T` and `V -> S -> U` respectively. We need to construct an expression `[(f, g)]` of type `V -> S -> T . U`.

Uncurrying `[f]` and `[g]` gives expression with types `V . S -> T` and `V . S -> U`. We can therefore form a function into the product `T . U`.

Currying now gives the correct answer:

> [(f, g)] = curry (uncurry [f], uncurry [g])

## Remaining Cases and Wrapping Up

I have omitted the analysis for the remaining two cases: uncurried expressions and functions out of a sum. As in the case of a curried function, each requires the use of an auxilliary function. In the case of uncurried expressions, the function is `assocl`. Eliminating a variable from a function out of a sum requires the function `distr`. These functions have types:

> assocl : A . (B . C) -> (A . B) . C
> 
> distr : A . (B + C) -> A . B + A . C

One can write definitions for these functions using only the constructions mentioned above. The upshot is that a single elimination step does not cause us to leave the class of expressions for which we can perform abstraction elimination, and we can continue eliminating variables all the way up the expression tree to the root, at which point we have removed all lambda expressions from the tree, and we are left with an equivalent expression in point-free style.

## References

[1] The Reader Monad and Abstraction Elimination, P. Pudlak, in The Monad.Reader Issue 17.te
