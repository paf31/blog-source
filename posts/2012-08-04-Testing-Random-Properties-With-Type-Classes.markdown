---
title: Testing Random Properties With Type Classes
author: Phil Freeman
date: 2012/08/04
description: Using type classes to test QuickCheck properties involving random number generation.
tags: Haskell, Testing
---

Consider this common technical interview question:

    Given a stream of bytes whose length is unknown, select a random byte from the stream using O(1) memory.

I chose this example because it is a non-trivial example of a problem involving random number generation.

One naive solution would be to store all of the bytes in a list in memory, waiting for the stream to close. This violates the last requirement of the problem though, which asks for a solution with constant memory utilization.

Let\'s start by defining a `Stream` datatype so that we can encode the problem in terms of finding a function of the correct type:

~~~{.haskell}
module Random where

import System.Random (Random, randomRIO)
import Control.Monad.Identity (Identity, runIdentity)
import Data.Ratio
import Test.QuickCheck

newtype Stream m a = Stream { runStream :: m (Maybe (NonEmptyStream m a)) }

type NonEmptyStream m a = (a, Stream m a)

empty :: (Monad m) => Stream m a
empty = Stream $ return Nothing

cons :: (Monad m) => a -> Stream m a -> Stream m a
cons a s = Stream $ return $ Just (a, s)

fromList :: (Monad m) => [a] -> Stream m a
fromList = foldr cons empty

fromList' :: (Monad m) => [a] -> NonEmptyStream m a
fromList' (x:xs) = (x, fromList xs)
~~~

Here, the monad `m` is used to represent the side effect of waiting for the next element in the `Stream`.

With that, we might aim to find a function of the following type:

~~~{.haskell}
select' :: NonEmptyStream IO a -> IO a
~~~

After a bit of thought, one arrives at the following solution:

~~~{.haskell}
select' (a, s) = select'' (return a) 1 s where
  select'' :: IO a -> Int -> Stream IO a -> IO a
  select'' a n s = do
    next <- runStream s
    case next of 
      Nothing -> a
      Just (a', s') -> select'' someA (n + 1) s' where
        someA = do i <- randomRIO (0, n) 
                   case i of 0 -> return a'
                             _ -> a
~~~

The idea is to keep two accumulator parameters - the first of type `IO a` represents some value of type `a` chosen with uniform probability from the values seen so far. The second of type `Int` is the number of values seen so far.

If we reach the end of the list, we return the first accumulated value. If not, we choose the new value with probability `1/n` and the value we had already chosen with probability `(n-1)/n`.

We can try this out in GHCi and the results look uniform enough:

    ghci> forM [1..10] $ const $ select' $ fromList' [1..10]
    [10,4,5,5,3,4,9,8,10,4]

Now you'd like to write some QuickCheck properties to verify that the results are indeed uniform.

The problem is that the function `select'` works in the IO monad, and is inherently non-deterministic. We could replace the use of `randomRIO` with a deterministic random function using a seed value, but then we would not be able to guarantee full coverage of the code. How many random samples would it take to gain confidence that your function indeed performs as expected?

The trick is to replace the `IO` monad with some monad living in a suitable typeclass.

Let's replace the call to `randomRIO` with a call to the new function `uniform`:

~~~{.haskell}
class (Monad r) => MonadRandom r where
  uniform :: (Int, Int) -> r Int
~~~

The new typeclass `MonadRandom` has at least one inhabitant that we know of, which is `IO`:

~~~{.haskell}
instance MonadRandom IO where
  uniform = randomRIO
~~~

Now instead of working with random values, let's identify the values with their probability distributions. This way, we do not lose any information by selecting a single value from the distribution.

Introduce the type `Dist a`, of probability distributions with values in type `a`:

~~~{.haskell}
newtype Dist a = Dist { runDist :: [(Rational, a)] } deriving (Show, Eq)
~~~

I've written about `Dist`'s `Monad` instance before, when I wrote about LINQ to Probability Distributions in C#:

~~~{.haskell}
instance Functor Dist where
  fmap f (Dist xs) = Dist $ fmap (\(p, x) -> (p, f x)) xs

instance Monad Dist where
  return x = Dist [(1, x)]
  (Dist xs) >>= f = normalize $ Dist $ do
    (p, x) <- xs
    (q, y) <- runDist $ f x
    return $ (p * q, y)
~~~

The function `normalize` appearing the definition of `>>=` ensures that the probabilities in the distribution sum to 1:

~~~{.haskell}
normalize :: Dist a -> Dist a
normalize d = Dist $ fmap (\(p, a) -> (p / total, a)) $ runDist d where
  total = sum $ map fst $ runDist d
~~~

In fact, `Dist` is also an instance of `MonadRandom`. The `uniform` function just returns a uniform distribution, as one would expect:

~~~{.haskell}
instance MonadRandom Dist where
  uniform (l, u) = Dist [ (1 % (toInteger $ u - l + 1), i) | i <- [l..u] ]
~~~

We can now rewrite the function `select'` in such a way that it works over an arbitrary monad in `MonadRandom`:

~~~{.haskell}
select :: (Functor m, Monad m, Monad r, MonadRandom r) => NonEmptyStream m a -> m (r a)
select (a, s) = select' (return a) 1 s where
  select' :: (Functor m, Monad m, Monad r, MonadRandom r) => r a -> Int -> Stream m a -> m (r a)
  select' r n s = do
    next <- runStream s
    case next of 
      Nothing -> return r
      Just (a, s') -> select' r1 (n + 1) s' where
        r1 = do i <- uniform (0, n) 
                case i of 0 -> return a
                          _ -> r
~~~

The new function `select` works just like `select'`, except that the result has an extra monadic layer:

    ghci> forM [1..10] $ const $ join $ select $ fromList' [1..10]
    [4,9,3,6,9,9,1,6,7,6]

However, now we can specialize `select` to the `Dist` monad for the purposes of testing our QuickCheck properties.

The idea is that since `select` is universally quantified in the monad `r`, we cannot cheat and use any specific knowledge we have about `r` to make our tests pass. If a test passes for one instance of `MonadRandom`, then we would expect the test to pass for any sensible instance of `MonadRandom`.

For example, let's write a property to check that selecting a random value from a `Stream` does not exclude any values:

~~~{.haskell}
testAllValuesPresent :: (Eq a) => [a] -> Bool
testAllValuesPresent xs = 
  all (flip elem values) xs where
  values = map snd 
    $ runDist
    $ runIdentity
    $ select 
    $ fromList' xs
~~~

We can test this property using QuickCheck:

    ghci> quickCheck testAllValuesPresent
    +++ OK, passed 100 tests.

Looks good. We could also write properties to verify that the choice is indeed uniform.

So by replacing the specific monad `IO` and working in the typeclass `MonadRandom`, we\'ve recovered testability for our random functions.
