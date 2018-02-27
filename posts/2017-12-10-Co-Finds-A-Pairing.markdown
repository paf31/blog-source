---
title: Co Finds a Pairing
author: Phil Freeman
date: 2017/12/10
description:
tags: Haskell
---

This is a quick note to explain something which I've hinted at in some of my previous posts, namely that the `Co` construction from Edward Kmett's [`kan-extensions`](https://hackage.haskell.org/package/kan-extensions) package can be used to construct a _pairing_ "for free" for a given functor. In fact, it constructs the most specific pairing in a certain sense, as we'll see.

## Pairings

First, a recap - a _pairing_ between two functors `f` and `g` is a function which combines two functors full of values to return a single result:

```purescript
type Pairing f g = forall a b. f (a -> b) -> g a -> b
```

Several functors can be usefully paired, such as

- `Identity` with `Identity`
- `Tuple a` with `(->) a`
- `State s` with `Store s`
- `Writer w` with `Traced w`
- `Free f` with `Cofree g` (assuming `f` and `g` can be paired)
- `Product f1 g1` with `Coproduct f2 g2` (assuming `f1` and `f2` pair, and `g1` and `g2` pair also)

Pairings are non-unique, since if I have a pairing between `f` and `g`, and a natural transformation `h ~> f`, then I can construct a pairing of `h` and `g`.

There is a trivial pairing between any `f` and `Const Void`:

```purescript
boring :: forall f. Pairing f (Const Void)
boring _ (Const v) = absurd v
```

## Pairings in terms of `Day`

It can be convenient to think of pairings in terms of `Day` convolution:

```purescript
Pairing f g
= forall a b. f (a -> b) -> g a -> b
~ (exists a. (f (a -> b), g a)) -> b
~ Day f g b -> b
~ Day f g ~> Identity
```

So a pairing is simply a natural transformation from `Day f g` to the identity functor.

`Day f` has [a right adjoint](https://github.com/paf31/purescript-day/blob/b2852de986384b50e2113b6efb3dd29eec39446e/generated-docs/Data/Functor/Day/Hom.md) `(f ⊸ -)` which acts as the internal hom functor in the symmetric monoidal category with `Day` as its tensor.

So we also have:

```purescript
Pairing f g
~ g ~> (f ⊸ Identity)
~ g ~> Co f
```

Thus, a pairing is a natural transformation from `g` to the functor `f ⊸ Identity` (which we might think of as "annihilating" `f` in the internal language of this symmetric monoidal category), which is isomorphic to `Co f`.

## A category of pairings

Let's fix a functor `f` and construct a category of functors which pair with `f`.

An object in this category will be a functor `g` along with a natural transformation `g ~> Co f` (i.e. a pairing of `f` and `g`).

A morphism between `g` and `h` will be a natural transformation `g ~> h` such that the obvious diagram commutes. This captures the idea above that `h` is more specific than `g`, since they differ by a natural transformation.

It is hopefully obvious that `id :: Co f ~> Co f` is the terminal object in this category, so in this sense, `Co f` is the most specific functor which pairs with `f`.

You might ask - what about functors such as `Maybe` which have no obvious pairing? Well in this case, we recover the `boring` pairing above, so every "other" pairing factors through the boring one.

## Putting this to use

How is this useful?

Well, for one, it can save us a step when working with pairings of free monads and cofree comonads, for example. Since `Co` has a lot of useful instances already, it is often not necessary to work with the paired functor explicitly.

I've also put this to use in my [recent paper](https://github.com/paf31/the-future-is-comonadic/blob/master/docs/main.pdf) which summarizes the ["comonads as spaces"](http://blog.functorial.com/posts/2016-08-07-Comonads-As-Spaces.html) work. In this case, it was convenient to frame user interfaces in terms of only a comonad, and to derive the corrsponding monad, instead of working with both.
