---
title: Stackless Coroutines in PureScript
author: Phil Freeman
date: 2015/07/31
description: Stack-safe coroutines using free monad transformers 
tags: Haskell
---

In this post, I'm going to describe how a neat trick known to the PureScript community allows us to write a 
stack-safe implementation of the _free monad transformer_ for a functor.

## Free Monads and Coroutines

Free monads are a useful tool in Haskell (and Scala, and PureScript, and other languages) when you want to separate
the specification of a `Monad` from its interpretation. We use a `Functor` to describe the terms we want to use, and
construct a `Monad` for free, which can be used to combine those terms.

Free monads can be used to construct models of _coroutines_, by using the base functor to specify the operations which 
can take place when a coroutine suspends:

```text
data Emit a = Emit o a

type Producer = Free Emit

emit :: o -> Generator Unit
emit o = liftF (Emit o unit)

emit3 :: Producer Int
emit3 = do
  emit 1
  emit 2
  emit 3
```

We can vary the underlying `Functor` to construct coroutines which produce values, consume values, fork child coroutines, 
join coroutines, and combinations of these.

This is described in [1], where free monad _transformers_ are used to build a library of composable coroutines and combinators 
which support effects in some base monad. If you haven't read this article yet, take some time to read it.

## Free Monads in Scala

Implementing free monads in languages like Scala and PureScript is tricky. A naive translation of the Haskell implementation 
works well for small computations, but we quickly run into the problem of _stack overflow_ when running larger computations
in free monads. Techniques such as monadic recursion become unusable.

Fortunately, the solution has been known to the Scala community for some time. [2] describes how to defer monadic binds in the
free monad, by capturing binds as a data structure. We can then write a tail recursive function to interpret this data structure of
binds, giving a free monad implementation which supports deep recursion.

This technique is also used in the `purescript-free` library, where the data constructor capturing the bind is named `Gosub`:

```text
newtype GosubF f a b = GosubF (Unit -> Free f b) (b -> Free f a)

data Free f a = Pure a
              | Free (f (Free f a))
              | Gosub (Exists (GosubF f a))
```

Here, we add the `Gosub` constructor which directly captures the arguments to a monadic bind, existentially hiding the return type `b`
of the inner action.

By translating the implementation in the paper, `purescript-free` builds a stack-safe free monad implementation for PureScript, which
has been used to construct several useful libraries.

However, in [2], when discussing the extension to a monad transformer, it is correctly observed that:

> In the present implementation in Scala, it's necessary to forego the parameterization on an additional monad, in order to
> preserve tail call elimination. Instead of being written as a monad transformer itself, Free could be transformed by a monad 
> transformer for the same effect.

That is, it's not clear how to extend the `Gosub` trick to the free monad _transformer_ if we want to be able to transform an arbitrary monad.

Additionally, the approach of putting a monad transformer _outside_ `Free` is not as useful as we'd like if we want to be able to use free monad
transformers in PureScript to describe coroutines, since we often want the `Eff` monad at the bottom of the stack, and there is no `EffT`
transformer!

Fortunately, all is not lost. A neat trick known to the PureScript community saves the day.

## Tail Recursive Monads

The PureScript compiler performs tail-call elimination for self-recursive functions, so that a function like

```text
pow :: Int -> Int -> Int
pow n p = go { accum: 1, power: p }
  where
  go { accum: acc, power: 0 } = acc
  go { accum: acc, power: p } = go { accum: acc * n, power: p - 1 }
```

gets compiled into an efficient `while` loop.

However, we do not get the same benefit when using monadic recursion:

```text
powWriter :: Int -> Int -> Writer Product Unit
powWriter n = go
  where
  go 0 = return unit
  go m = do
    tell n
    go (m - 1)
```

However, we can refactor the original function to isolate the recursive function call:

```text
pow :: Int -> Int -> Int
pow n p = tailRec go { accum: 1, power: p }
  where
  go :: _ -> Either _ Number
  go { accum: acc, power: 0 } = Right acc
  go { accum: acc, power: p } = Left { accum: acc * n, power: p - 1 }
```

where the `tailRec` function is defined in the `Control.Monad.Rec.Class` module, with type:

```text
tailRec :: forall a b. (a -> Either a b) -> a -> b
```

In the body of the loop, instead of calling the `go` function recursively, we return a value using the `Left` constructor. To break from 
the loop, we use the `Right` constructor.

The type of `tailRec` can be generalized to several monad transformers from the `purescript-transformers` library using the 
following type class in the `purescript-tailrec` library:

```text
class (Monad m) <= MonadRec m where
  tailRecM :: forall a b. (a -> m (Either a b)) -> a -> m b
```

It turns out that this class of "tail recursive monads" is large enough to be useful, including base monads like `Eff` and `Identity`, and
closed under transformers like `StateT`, `ErrorT`, `WriterT` etc. It is also enough to rescue the implementation of `FreeT`.

## Stack-Safe Free Monad Transformers

We can steal the `Gosub` trick from the `Free` monad implementation and apply it to our proposed `FreeT`:

```text
data GosubF f m b a = GosubF (Unit -> FreeT f m a) (a -> FreeT f m b)

data FreeT f m a 
  = FreeT (Unit -> m (Either a (f (FreeT f m a)))) 
  | Gosub (Exists (GosubF f m a))
```

The instances for `Monad` and friends generalize nicely from `Free` to `FreeT`, composing binds by nesting `Gosub` constructors. This allows
us to build computations safely using recursion. The difficult problem is how to _run_ a computation once it has been built.

Instead of allowing interpretation in any monad, we only support interpretation in one of our tail recursive monads. We can reduce
the process of interpreting the computation to a tail recursive function in that monad:

```text
runFreeT :: forall f m a. (Functor f, MonadRec m) => (forall a. f a -> m a) -> FreeT f m a -> m a
```

See the [implementation of `runFreeT`](https://github.com/paf31/purescript-freet/blob/cdbaf6ddfcd97eb6816f6854672cc5126d36c603/src/Control/Monad/Free/Trans.purs#L102-L109) for more details.

## Stackless Coroutines

Given a stack-safe implementation of the free monad transformer, it becomes simple to translate the coroutines from [1] into PureScript. Here is
an example of a producer/consumer pair described using two coroutines:

```text
producer :: forall m. (Monad m) => Producer Int m Unit
producer = go 0
  where
  go i = do emit i
            go (i + 1)
    
consumer :: forall a. (Show a) => Consumer a (Eff _) Unit
consumer = forever do
  s <- await
  lift (log s)
```

Despite the monadic recursion in `producer`, these coroutines can be connected and run using a constant amount of stack, thanks to the 
combination of tricks above:

```text
main = runProcess (producer $$ consumer)
```

The `purescript-coroutines` library supports a handful of combinators for connecting producers, consumers and transformers,
as well as more powerful, generic coroutine machinery taken from [1].

I hope that this library will become the basis of an ecosystem of streaming utility libraries in PureScript, supporting streaming access
to resources like the filesystem, databases and web services. If you're interested in contributing, join us on the #purescript Freenode 
channel to discuss it!

## References

1. _Coroutine Pipelines_, by Mario Blazevic, in _The Monad Reader_, _Issue 19_.
1. _Stackless Scala With Free Monads_, by Runar Oli Bjarnason.
