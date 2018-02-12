---
title: Extensible Coeffects
author: Phil Freeman
date: 2018/02/11
description: 
tags: Haskell
---

## Introduction

Recently, I've been experimenting with _coeffects_ in PureScript. I'll give an introduction to some of the ideas here, or if you'd like to see the code, you can [find it on GitHub](https://github.com/paf31/purescript-smash).

## Day Convolution and Combining Interpreters

Given a comonad `w`, one way of thinking about a value of type `w a` is as a simulation of a process where, at each step, we can ask for a value of type `a` which might indicate some portion of the current state. I've written about this intuition for comonads before in my ["comonads as spaces"](http://blog.functorial.com/posts/2016-08-07-Comonads-As-Spaces.html) series of blog posts.

From the comonad `w`, Edward Kmett [has shown](http://comonad.com/reader/2011/monads-from-comonads/) how to construct a monad `Co w`. The `Co w` monad is the most specific monad for which there is a _pairing_ with the comonad `w`, which I [wrote about here](http://blog.functorial.com/posts/2017-12-10-Co-Finds-A-Pairing.html).

So another way of looking at comonads is as _interpreters_ for the actions of these monads.

Previously, I wrote about how [the Day convolution of two comonads is also a comonad](http://blog.functorial.com/posts/2016-08-08-Comonad-And-Day-Convolution.html), and how, in fact, Day convolution gives the category of comonads over Hask the structure of a (closed) symmetric monoidal category. 

It's interesting to think about Day convolution in terms of our intution for comonads as interpreters. There are natural transformations from `Co w1` and `Co w2` to `Co (Day w1 w2)`, so the monad constructed from `Day w1 w2` supports the operations of _both_ the `Co w1` and `Co w2` monads. 

## Some Examples

The `EnvT`, `StoreT` and `TracedT` comonad transformers exhibit a common pattern:

- The `Co (Env e)` monad is isomorphic to `Reader e`. The Day convolution `Day (Env e) w` is isomorphic to `EnvT e w`.
- The `Co (Store s)` monad is isomorphic to `State s`. The Day convolution `Day (Store s) w` is isomorphic to `StoreT s w`.
- The `Co (Traced m)` monad is isomorphic to `Writer m`. The Day convolution `Day (Traced m) w` is isomorphic to `TracedT m w`.

The `Co (Cofree f)` monad embeds the actions of the `Free (Co f)` monad. However, the `Day (Cofree f) w` is not always isomorphic to `CofreeT f w`.

## Extensible Coeffects and Extensible Effects

All of the above leads us to a simple model of "extensible coeffects": simply take a collection of comonads which model the coeffects we are interested in, and use Day convolution to create a new comonad which supports all of the desired coeffects. For many common comonads listed above, taking the Day convolution corresponds to using comonad transformers, and we can even use `Co` to contruct a monad with which to navigate this new, combined state space. Our new monad will embed the actions of all of the monads we could have constructed from the individual monads, and we could use techniques in the style of [Data types à la carte](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.101.4131) to make this style really "extensible".

Let's compare this approach to a simplified recipe for extensible effects:

- Use functors to describe the effects we would like to model.
- Form the coproduct of these functors
- Form the free monad (transformer) over the coproduct.
- Lift the actions defined by each functor into the free monad (transformer).
- Handlers peel of single effects, interpreting a free monad for a coproduct in terms of a free monad for a slightly smaller coproduct, optionally modifying the return type.

The extensible coeffects approach supports most of the features of this model of extensible effects, since a Day convolution of cofree comonads is isomorphic to a cofree comonad for the product of the two underlying functors, and so the corresponding monad is a free monad of a coproduct of functors. We can probably even improve performance by using more optimal comonads such as `Store` in place of `Cofree` comonads where possible.

However, there is one gap which becomes apparent when using coeffects in this way. Extensible effect libraries are usually defined in terms of some base monad such as `IO`. Suppose we want to model a connection to a database - we might define appropriate effects, and the handlers would use the features of some `IO`-supporting base monad to actually talk to the database. With naive extensible coeffects, this is just not possible. There is no comonad `w` for which `Co w` is isomorphic to `IO`.

The benefit of coeffects is that Day convolution is a symmetric construction, so we never need to worry about the order in which we handle our coeffects, unlike when working with monad transformers or extensible effects. The price we pay is a loss of expressivity, since there are some monads we cannot model (such as `IO`, but even more basic things like `Either e`).

However, all is not lost, and we can recover expressivity by combining monads with comonads.

## Interleaving monadic effects

In order to interleave additional monadic effects, we must move from the `Co` monad to the `CoT` monad transformer. Edward detailed this transformer in [this blog post](http://comonad.com/reader/2011/monad-transformers-from-comonads/). The definition is as follows:

```haskell
data CoT w m a = CoT { runCoT :: forall r. w (a -> m r) -> m r }

type Co w = CoT w Identity
```

Now we can `lift` effects from the base monad into the `CoT` transformer, and thereby interleave such effects with those arising from our Day convolution of comonads.

However, we can do more. Consider, for example, how to create a new coeffect for reading from and writing to the terminal. We might do this using `Cofree`:

```haskell
data TerminalF a = TerminalF
  { readLine :: (String, a)
  , writeLine :: String -> a
  }
  
type Terminal = Cofree TerminalF
```

An interpreter for our terminal coeffect might have type `Terminal (IO ())`, but we immediately run into a problem. We must produce a `String` immediately, in order to respond to a possible `readLine` request, without the option to perform any IO.

If we consider the definition of the `Cofree` comonad:

```purescript
data Cofree f a = Cofree a (f (Cofree f a))
```

we realize that we really need to wedge the base monad `m` in front of the occurence of `f`:

```purescript
data Cofree f m a = Cofree a (m (f (Cofree f a)))
```

Instead of using this type directly, we can notice that this type is isomorphic to `Cofree (Compose m f) a` - that is, it is a regular old cofree comonad, where we have simply rolled the monad into the base functor. We can design combinators to take advantage of the monad. For example, we can lift a functor's worth of actions:

```purescript
liftCoF :: (Monad m, Functor f) => Co f a -> CoT (Cofree (Compose m f)) m a
liftCoF co = CoT $ \w -> do
  x <- getCompose (unwrap w)
  runIdentity (runCoT co (map (\y -> Identity . extract y) x))
```

So, the `CoT (Cofree (Compose m f)) m` monad combines the features of the `Free (Co f)` and `m` monads.

As we will see next, this combination can be quite useful.

## Coroutines, Comonadically

I've [written previously](http://blog.functorial.com/posts/2015-07-31-Stackless-PureScript.html) about using free monad transformers to build the `purescript-coroutines` library.

Free monad transformers are a very convenient way to build coroutines, but they have a limitation: in a producer-consumer scenario, either the producer or consumer can terminate the pipeline, causing the other to stop early. We can prevent termination by using a `Void` return value, but there is an interesting alternative.

We can create a comonadic consumer to pair with a monadic producer:

```haskell
-- | A consumer comonad, with effects from m interleaved at each step
type CoConsumer e m = Cofree (Compose m ((->) e))

-- | Producer monad
type Producer e m = CoT (CoConsumer e m) m
       
-- | Emit a value
emit :: forall e m. e -> Producer e m ()
emit e = liftCoF . CoT $ \f -> f e ()
```

The comonadic consumer must continue consuming values until the producer signals termination.

Alternatively, we can create a comonadic producer to pair with a monadic consumer:

```haskell
-- | A producer comonad, with effects from m interleaved at each step
type CoProducer e m = Cofree (Compose m (Tuple e))

-- | Consumer monad
type Consumer e m = CoT (CoProducer e m) m
       
-- | Await a value
await :: forall e m. Consumer e m e
await = liftCoF . CoT $ \(e, f) -> f e
```

In this scenario, it is the consumer which controls termination, and the producer must continue producing values indefinitely, if necessary.

## Row Polymorphism

Finally, I'd like to show how we can use row polymorphism to really emphasize the "extensible" in extensible coeffects.

The [`purescript-run` library](https://github.com/natefaubion/purescript-run/) by Nathan Faubion uses a rather ingenious idea for encoding extensible effects using PureScript's row polymorphism. As above, an effect monad is defined using a free monad, but instead of using a coproduct of base functors, the library uses higher-kinded polymorphic variants (at kind `* -> *`). Row polymorphism allows us to talk about only the effects we care about, and to quantify over the rest. This gives us a particularly nice API to work with, since type inference tends to do the right thing, and we never end up having to deal with deeply nested coproduct types.

I borrow this idea in my coeffects library. Instead of a variant, which we can think of as an anonymous n-ary functor coproduct, the library defines an n-ary Day convolution of functors based on a row type. As in `purescript-run`, we can use row polymorphism to only talk about the coeffects we care about, and quantify over the rest. See the [library implementation](https://github.com/paf31/purescript-smash/) if you are interested in the details.

_Aside:_ This n-ary Day convolution product acts like a record type in the (closed) symmetric monoidal category of functors with Day convolution. Just as the labels in a record type can be used to define lenses into the corresponding properties, or as the labels in a polymorphic variant can be used to define the corresponding prisms, we can use the labels in our row to represent "Day optics", which I wrote about before  briefly [here](http://blog.functorial.com/posts/2016-08-08-Comonad-And-Day-Convolution.html). This provides another interesting way to work polymorphically with coeffects, akin to something like "classy lenses".

## Conclusion

Extensible coeffects provide an interesting alternative to extensible effects, with a curious set of trade-offs. On the one hand, using Day convolution means that all of our coeffects commute, but on the other we lose some expressivity by working with comonads instead of monads. However, we can recover quite a bit of expressivity by working over an additional base monad and writing combinators to support this. PureScript's row polymorphism makes for a pleasant API when working with coeffects polymorphically.

Thank you to Nicholas Scheel for providing valuable feedback on this post.
