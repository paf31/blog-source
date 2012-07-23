---
title: What Makes the Free Monad Free?
author: Phil Freeman
date: 2012/07/22
description: An explanation of the term 'free' monad in terms of adjunctions.
tags: Haskell, Category Theory
---

I\'d like to give an explanation of the word \'free\' in \'free monad\'. When we speak of free objects (free groups, free monoids), what we are really doing is asserting the existence of an adjoint functor for a forgetful functor. Therefore, we expect that the construction of free monads should be adjoint to some functor.

Let's recap the definition of a free monad.

~~~{.haskell}
{-# LANGUAGE Rank2Types #-}

import Control.Monad (join)

data Free f a = Return a | Bind (f (Free f a))
~~~

The free monad for a functor `f` is given by the fixed point of the bifunctor `X -> A -> F (X + A)` where the fixed point is taken over `X`.

Thus, the free monad describes structures whose pattern of recursion looks like `f`, but where whole substructures can be replaced with a leaf of type `A`. The free monad defines a functor in `A`, and a monad, for any functor `f`:

~~~{.haskell}
instance (Functor f) => Functor (Free f) where
  fmap f (Return x) = Return (f x)
  fmap f (Bind xs) = Bind (fmap (fmap f) xs)

instance (Functor f) => Monad (Free f) where
  return = Return
  (Return x) >>= f = f x
  (Bind xs) >>= f = Bind $ fmap (>>= f) xs
~~~

As a mapping from functors to monads, `Free` is also a functor, from the category of endofunctors to the category of monads and monad morphisms. A monad morphism is just a natural transformation which commutes with the return and bind operations.

~~~{.haskell}
mapFree :: (Functor f, Functor g) => (forall a. f a -> g a) -> Free f a -> Free g a
mapFree f (Return x) = Return x
mapFree f (Bind xs) = Bind $ f $ fmap (mapFree f) xs
~~~

Now we can show that this functor from endofunctors to monads is actually left adjoint to the forgetful functor which takes a monad and forgets the monadic structure.

We need to give a natural equivalence between hom-sets `Free f ~> m` in the category of monads and monad morphisms, and `f -> Forget m` in the category of endofunctors.

That is, we need to define mappings with the following signatures:

~~~{.haskell}
leftAdjunct :: (Functor f, Monad m) => (forall a. f a -> m a) -> Free f a -> m a
rightAdjunct :: (Functor f, Monad m) => (forall a. Free f a -> m a) -> f a -> m a
~~~

And then show that these are inverses.

Let\'s define these functions by following the types:

~~~{.haskell}
leftAdjunct _ (Return x) = return x
leftAdjunct phi (Bind xs) = join $ phi $ fmap (leftAdjunct phi) xs

rightAdjunct psi = psi . Bind . (fmap Return)
~~~

Using parametricity, it is not hard to see that `leftAdjunct` followed by `rightAdjunct` must be the identity since its type is 

~~~
forall f m. (Functor f, Monad m) => (forall a. f a -> m a) -> (forall a. f a -> m a)
~~~

In the other direction, we have

~~~
leftAdjunct (rightAdjunct f) (Return x) = return x
~~~

And `f (Return x) = return x` because `f` is a monad morphism.

Also,

~~~
leftAdjunct (rightAdjunct f) (Bind xs) 
  (definition of leftAdjunct)
  = join $ rightAdjunct f $ fmap (leftAdjunct (rightAdjunct f)) xs
  (definition of rightAdjunct)
  = join $ f $ Bind $ fmap Return $ fmap (leftAdjunct (rightAdjunct f)) xs
  (since f is a monad morphism)
  = f $ join $ Bind $ fmap Return $ fmap (leftAdjunct (rightAdjunct f)) xs
  (definition of join)
  = f $ Bind $ fmap (leftAdjunct (rightAdjunct f)) xs
  (by structural induction on Free f)
  = f (Bind xs)
~~~

so `leftAdjunct (rightAdjunct f)` is equal to `f` and `Free` is indeed left adjoint to the forgetful functor to `Endo C`.

The case of cofree comonads is very similar: one can show that the construction of a cofree comonad over a functor `f` gives a functor from `Endo C` to comonads and comonad morphisms which is right adjoint to the forgetful functor to `Endo C`.
