---
title: Doubly-Recursive Functions, Generically
author: Phil Freeman
date: 2012/02/12
description: A generic formulation of 'doubly-recursive' functions in Haskell
tags: Haskell, Recursion
---

Consider the following recursive function definitions. What do they have in common?

~~~{.haskell}
-- equals 0 0 = True
-- equals 0 n = False
-- equals n 0 = False
-- equals n m = equals (n - 1) (n - 2)

-- unify (Unknown n) (Unknown m) = [(n, m)]
-- unify (Arrow x y) (Arrow w z) = (unify x w) ++ (unify y z)
-- unify _ _ = error "Cannot unify"

-- elementAt 0 (x:xs) = x
-- elementAt n (x:xs) = elementAt (n - 1) xs
-- elementAt _ _ = error "Index out of bounds"
~~~

Well, all three functions take two arguments, and each one is defined recursively. Just as we can define (cata/ana)morphisms for arbitrary base functors, we can unify these three functions under a single abstraction which works over a pair of functors.

~~~{.haskell}
{-# LANGUAGE Rank2Types, DeriveFunctor #-}
~~~

Let's define a fixed point type for an arbitrary functor f as follows:

~~~{.haskell}
newtype Rec f = In { out :: f (Rec f) }
~~~

We can define folds and unfolds for a functor f in a uniform way as usual.

~~~{.haskell}
fold phi = phi . fmap (fold phi) . out
unfold psi = In . fmap (unfold psi) . psi
~~~

Now look at the definition of the function equals above. Consider the points where the function calls itself recursively. This happens only in the last case, where both sides contribute a smaller term. In general, we can only call a doubly recursive function recursively if we have two arguments to call the function with.

This observation motivates the following definition. For functors `f` and `g`, define the following combination `f # g` of the two:

~~~
-- (f # g) x = exists a b. (a -> b -> x) (f a) (g b)
~~~

Now, type `x` appears in `(f # g) x` in those places where there is an `a` contributed from `f`, and a `b` contributed from `g`.

For example, if we define a base functor for the type of natural numbers:

~~~{.haskell}
data Nat t = Zero | Succ t deriving (Show, Functor)
~~~

then we have

~~~
-- (Nat # Nat) t = exists a b. (a -> b -> x) (Nat a) (Nat b)
--               ~ exists a b. (a -> b -> x) (1 + a) (1 + b)
--               ~ exists a b. (a -> b -> x) (1 + a + b + a . b)
--               ~ 1 + 1 + 1 + x
~~~

since we cannot eliminate the summands `a` and `b` from the existential using only the function of type `a -> b -> x`.

Let\'s name the three unit constructors here `ZZ`, `ZS` and `SZ` depending on the two multiplicands from which they originated in the above isomorphism.

Now we can define a function equals':

~~~
-- equals' :: (Nat # Nat) t -> t
-- equals' ZZ = True
-- equals' ZS = False
-- equals' SZ = False
-- equals' SS t = t
~~~

This looks a lot like the definition of `equals` given at the top of the article.

With this motivating case out of the way, we now want to use a function of type `(f # g) t -> t` to define a doubly recursive function over arbitrary functors `f` and `g`:

~~~
-- (f # g) t -> t ~ (exists a b. (a -> b -> x) (f a) (g b)) -> t
--                ~ (forall a b. (a -> b -> x) -> f a -> g b -> t)
~~~

This is the type of algebra that we will use to fold a pair of structures in parallel.

Finally, let\'s define the parallel fold function:

~~~{.haskell}
parFold :: (forall a b. (a -> b -> x) -> f a -> g b -> x) -> Rec f -> Rec g -> x
parFold phi x y = (phi $ parFold phi) (out x) (out y)
~~~

The definition is quite simple: we unwrap each of the arguments by one level, and combine them by using the algebra function, passing the fold itself to the algebra function, so that it may call the fold recursively for smaller arguments. This looks a lot like Mendler style recursion, but using two arguments in parallel.

While we\'re at it, let\'s define a parallel unfold function, which is dual to the definition above:

~~~{.haskell}
parUnfold :: (forall a b. (x -> (a, b)) -> x -> (f a, g b)) -> x -> (Rec f, Rec g)
parUnfold psi x = let (a, b) = (psi $ parUnfold psi) x in (In a, In b)
~~~

Now we can define the `equals` and `unify` functions above as instances of `parFold`:

~~~{.haskell}
equals = parFold equals' where
  equals' _ Zero Zero = True
  equals' _ Zero _ = False
  equals' _ _ Zero = False
  equals' f (Succ x) (Succ y) = f x y

data Ty t = Unknown Int | Arrow t t deriving (Show, Functor)

unify = parFold unify' where
  unify' _ (Unknown n) (Unknown m) = Just [(n, m)]
  unify' f (Arrow x y) (Arrow z w) = do cs1 <- (f x z); cs2 <- (f y w); return $ cs1 ++ cs2
  unify' _ _ _ = Nothing
~~~

The third example shows that the two recursive structures do not have to be defined over the same base functor:

~~~{.haskell}
data List a t = Empty | Cons a t deriving (Show, Functor)

elementAt = parFold elementAt' where
  elementAt' _ Zero (Cons a t) = Just a
  elementAt' f (Succ n) (Cons a t) = f n t
  elementAt' _ _ _ = Nothing
~~~
