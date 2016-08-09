---
title: Comonads and Day Convolution
author: Phil Freeman
date: 2016/08/08
description:
tags: Haskell
---

### Day Convolution

The _Day convolution_ of two functors is another functor, defined as follows:

```haskell
data Day f g a where
  Day :: (x -> y -> a) -> f x -> g y -> Day f g a
```

There is an implementation of `Day` in [Edward Kmett's `kan-extensions` package](https://hackage.haskell.org/package/kan-extensions-5.0.1/docs/Data-Functor-Day.html) on Hackage.

### Day Convolution of Comonads

While writing my [last post](2016-08-07-Comonads-As-Spaces.html), I was looking for a way to combine comonads, and found out some interesting things about Day convolution in the process.

It turns out that there is a valid `Comonad` instance for `Day f g` whenever `f` and `g` are
themselves `Comonad`s:

```haskell
instance Functor (Day f g) where
  fmap g (Day f x y) = Day (\a b -> g (f a b)) x y

instance (Comonad f, Comonad g) => Comonad (Day f g) where
  extract (Day f x y) = f (extract x) (extract y)
  duplicate (Day f x y) = Day (Day f) (duplicate x) (duplicate y)
```

That's interesting, but what's more interesting is that `Day f` is even a comonad transformer:

```haskell
instance Comonad f => ComonadTrans (Day f) where
  lower (Day f x y) = fmap (f (extract x)) y
```

So for every comonad, we have a ([symmetric](https://hackage.haskell.org/package/kan-extensions-5.0.1/docs/Data-Functor-Day.html#v:swapped)) comonad transformer, for free!

These instances are now available [in the `kan-extensions` package](https://github.com/ekmett/kan-extensions/commit/c77c6ec2180ddbd1d1bf82c308ef4a36a6702255).

### A Free Theorem

In order to prove the comonad laws, we'll need to apply a certain free theorem.

Consider how we might observe the state of a comonadic value of type `Day f g`. We would need a
function `f` of type `Day f g a -> r` for some result type `r`. Now, this is isomorphic to:

```haskell
f :: Day f g a -> r
   ~ (exists x y. (x -> y -> a, f x, g y) -> r
   ~ forall x y. (x -> y -> a) -> f x -> g y -> r
```

If we plug the type of `f` into the [online free theorem generator](http://www-ps.iai.uni-bonn.de/cgi-bin/free-theorems-webui.cgi), then we find out that its free theorem is (after some tidying):

```haskell
f (\a b -> g (f1 a) (f2 b)) x y = f g (fmap f1 x) (fmap f2 y)
```

This theorem tells us that we can either apply a function in one of the functorial structures `x` or `y`,
or defer it until the extraction function `g`. We won't be able to observe the difference, so we
should consider these two values to be equal:

```haskell
Day (\a b -> g (f1 a) (f2 b)) x y = Day g (fmap f1 x) (fmap f2 y)
```

This theorem will come in very useful soon.

### Proving the Comonad Laws

The first comonad law states that

```haskell
extract . duplicate = id
```

This one is simple to prove:

```haskell
extract (duplicate (Day f x y))
  = extract (Day (Day f) (duplicate x) (duplicate y))
  = Day f (extract (duplicate x)) (extract (duplicate y))
  = Day f x y
```

The second comonad law states that

```haskell
fmap extract . duplicate = id
```

This is slightly trickier, relying on the free theorem above:

```haskell
fmap extract (duplicate (Day f x y))
  = fmap extract (Day (Day f) (duplicate x) (duplicate y))
  = Day (\a b -> extract (Day f a b)) (duplicate x) (duplicate y)
  = Day (\a b -> f (extract a) (extract b)) (duplicate x) (duplicate y)
  = Day f (fmap extract (duplicate x)) (fmap extract (duplicate y))
  = Day f x y
```

Notice how we use the free theorem in the fifth line, to move the `extract` from the first argument to the second and third arguments.

The final law states that

```haskell
fmap duplicate . duplicate = duplicate . duplicate
```

Again, we'll need the free theorem:

```haskell
duplicate (duplicate (Day f x y))
  = duplicate (Day (Day f) (duplicate x) (duplicate y))
  = Day (Day (Day f)) (duplicate (duplicate x)) (duplicate (duplicate y))
  = Day (Day (Day f)) (fmap duplicate (duplicate x)) (fmap duplicate (duplicate y))
  = Day (\a b -> duplicate (Day (Day f) a b)) (duplicate x) (duplicate y)
  = fmap duplicate (Day (Day f) (duplicate x) (duplicate y))
  = fmap duplicate (duplicate (Day f x y))
```

Success!

Now let's check the two `ComonadTrans` laws. The first says that

```haskell
extract . lower = extract
```

This one is straightforward:

```haskell
extract (lower (Day f x y))
= extract (fmap (f (extract x)) y)
= f (extract x) (extract y)
= extract (Day f x y)
```

The second `ComonadTrans` law states that

```haskell
duplicate . lower = lower . fmap lower . duplicate
```

This one is a more involved application of the comonad laws of the base monad:

```haskell
lower (fmap lower (duplicate (Day f x y)))
  = lower (fmap lower (Day (Day f) (duplicate x) (duplicate y)))
  = lower (Day (\a b -> lower (Day f a b)) (duplicate x) (duplicate y))
  = lower (Day (\a b -> fmap (f (extract a)) b) (duplicate x) (duplicate y))
  = fmap ((\a b -> fmap (f (extract a)) b) (extract (duplicate x)) (duplicate y)
  = fmap ((\a b -> fmap (f (extract a)) b) x) (duplicate y)
  = fmap (fmap (f (extract x))) (duplicate y)
  = duplicate (fmap (f (extract x)) y)
  = duplicate (lower (Day f x y))
```

That's it! `Day f` is a law-abiding comonad transformer for any `Comonad` `f`.

### Comonad Optics

In fact, `Day` provides a _symmetric monoidal product_ over the category of Haskell comonads. The unit of this product is the `Identity` functor.

`kan-extensions` defines the functions which provide this structure, but I won't cover the proof here.

In [his video on monad transformer lenses](https://www.youtube.com/watch?v=Bxcz23GOJqc), Edward Kmett makes a case for generalized profunctor lenses whenever we have a monoidal category. In the case of monads, the monoidal product was `Compose`. However, `Compose` is not symmetric, so we needed to separate left lenses from right lenses. However, in the case of comonads, we can use `Day` as our monoidal product, and `Day` _is_ symmetric, making things a little simpler.

If we form the class of "strong" profunctors in the comonad category, we can derive a sensible notion of an optic which can focus on a component in a larger comonad which is isomorphic to a Day convolution of smaller comonads:

```haskell
type (~>) f g = forall a. f a -> g a

class Profunctor p where
  dimap :: (f ~> g)
        -> (h ~> i)
        -> p i f
        -> p h g

class Convoluted p where
  convoluted :: (Comonad f, Comonad g, Comonad w)
             => p f g
             -> p (Day f w) (Day g w)

type Optic s t a b = Convoluted p => p a b -> p s t
```

Now for the interesting part: it turns out that many standard comonad transformers are _already_
isomorphic to `Day f` for some `f`, so we can use these optics to easily operate inside comonad transformer stacks.

For example, consider `StoreT`:

```haskell
data StoreT s w a = StoreT (s, w (s -> a))
```

Now, it turns out this is isomorphic to `Day (Store s)` as follows:

```haskell
Day (Store s) w a
  ~ exists x y. (x -> y -> a, Store s x, w y)
  ~ exists x y. (x -> y -> a, s, s -> x, w y)
  ~ exists y. (s, s -> y -> a, w y)
  ~ exists y. (s, y -> s -> a, w y)
  ~ (s, w (s -> a))
  ~ StoreT s w a
```

Similarly, for `EnvT`:

```haskell
Day (Env e) w a
  ~ exists x y. (x -> y -> a, Env e x, w y)
  ~ exists x y. (x -> y -> a, e, x, w y)
  ~ exists y. (y -> a, e, w y)
  ~ (e, w a)
  ~ EnvT e w a
```

And `TracedT`:

```haskell
Day (Traced m) w a
  ~ exists x y. (x -> y -> a, Traced m x, w y)
  ~ exists x y. (x -> y -> a, m -> x, w y)
  ~ exists y. (m -> y -> a, w y)
  ~ w (m -> a)
  ~ TracedT m w a
```

So now we can write optics which focus on a `Store` inside a `StoreT`, or on the comonad transformed by `StoreT`, and similar optics for `EnvT` and `TracedT`.

I have a [sketch](https://github.com/paf31/purescript-functor-optics/blob/c3745a59588a80aae67a089c5e1215751b977ba6/src/Data/Functor/Optic.purs#L49-L80) of an implementation of these optics in PureScript.

It's not entirely clear to me what we can do with these optics just yet, but it seems like it should at least be possible to [lift actions for the corresponding monads](https://github.com/paf31/purescript-comonad-ui/blob/a644d0d39c39c5c58a0f55804e956e133f237dec/src/Main.purs#L152-L156) whenever we have a comonad optic which identifies one comonad as a Day convolution of a smaller comonad. I'm hoping this can be useful when using [comonads to describe user interfaces](2016-08-07-Comonads-As-Spaces.html).
