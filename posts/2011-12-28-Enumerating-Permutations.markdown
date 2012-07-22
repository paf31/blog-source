---
title: Enumerating Permutations
author: Phil Freeman
date: 2011/12/28
---

Consider the following Haskell function which enumerates permutations of a given length:

~~~{.haskell}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

import Data.List

perms 0 = [[]]
perms n = [ insertAt i n p | p <- perms (n - 1), i <- [0..n-1] ]

insertAt 0 x xs = x:xs
insertAt n x (x':xs) = x':xs' where xs' = insertAt (n - 1) x xs
~~~

The goal of this post is to derive a partial inverse `indexOfPerm` to the indexing function `perms n !!` as an exercise in equational reasoning in Haskell. That is, we seek a function such that for all `i`:

~~~
-- indexOfPerm :: Int -> [Int] -> Int
-- indexOfPerm n (perms n !! i) = i
~~~

This will serve as a specification of the function `indexOfPerm`.

Expanding the definition of perms at zero gives the following:

~~~
-- indexOfPerm 0 []
--   { Definition of perms }
--   = indexOfPerm 0 (perms 0 !! 0)
--   { By assumption }
--   = 0
~~~

Expanding the recursive definition of `perms` gives the following:

~~~
-- indexOfPerm n (perms n !! (j * n + k))
--  { Definition of perms }
--  = indexOfPerm n (insertAt j n (perms (n - 1) !! k))
--  { Let xs = insertAt j n (perms (n - 1) !! k) }
--  = indexOfPerm n xs
--  { By assumption }
--  = j * n + k
~~~

Suppose we can find a function `extract` which satisfies the following:

~~~
-- extract :: Int -> [Int] -> (Int, [Int])
-- extract n (insertAt i n xs) = (i, xs)
~~~

Now calculate as follows:

~~~
-- extract n xs
--   { Definition of xs }
--   = extract n (insertAt j n (perms (n - 1) !! k) )
--   { Definition of extract }
--   = (j, perms (n - 1) !! k)
--   { Let xs' = perms (n - 1) !! k) }
--     so that k = indexOfPerm (n - 1) xs' }
--   = (j, xs')
-- 
-- indexOfPerm n xs
--   { From earlier }
--   = j * n + k
--   { Expressing k in terms of xs' }
--   = j * n + indexOfPerm (n - 1) xs'
~~~

We can now define `indexOfPerm` as follows:

~~~
indexOfPerm 0 [] = 0
indexOfPerm n xs = n * (indexOfPerm (n - 1) xs') + j
  where (j, xs') = extract n xs
~~~

It remains to compute the function `extract`. Expanding the definition of `insertAt` at zero gives:

~~~
-- extract x (x:xs)
--   { Definition of insertAt }
--   = extract x (insertAt 0 xs)
--   { By assumption }
--   = (0, xs)
~~~

Expanding the recursive definition of `insertAt` gives:

~~~
-- extract x (x':xs)
--   { Assume xs = insertAt i x xs'
--     so that (i, xs') = extract x xs }
--   = extract x (x':insertAt i x xs')
--   { Definition of insertAt }
--   = extract x (insertAt (i + 1) x x':xs'))
--   { By assumption }
--   = (i + 1, x':xs')
~~~

Now we can define `extract` as follows:

~~~{.haskell}
extract x (x':xs) | x == x'    = (0, xs)
                  | otherwise  = (i + 1, x':xs')
  where (i, xs') = extract x xs 
~~~

One can check that the relation that we are interested in between `insertAt` and `extract` actually holds.

## Generating Permutations

We can now combine `perms` and `indexOf` to give a function `nextPerm` which generates the next permutation in the list `perms n`:

~~~{.haskell}
fact 0 = 1
fact n = n * fact (n - 1)

nextPerm' n xs = perms n !! ((1 + indexOfPerm n xs) `mod` (fact n))
~~~

However, we can rewrite this function by fusing the definition of `perms` with the definition of `indexOfPerm`:

~~~
-- nextPerm 0 []
--   { Definition of nextPerm }
--   = perms 0 !! ((1 + indexOfPerm 0 []) mod (fact 0))
--   { Definition of indexOfPerm }
--   = perms 0 !! (1 mod 1)
--   { Definition of perms }
--   = []
~~~

The recursive case is only slightly more tricky. We divide into two cases.

~~~
-- nextPerm n xs
--     { Definition of nextPerm }
--     = perms n !! ((1 + indexOfPerm n xs) mod (fact n))
--     { Let (j, xs') = extract n xs }
--     = [ insertAt i n p | p <- perms (n - 1), i <- [0..n- 1] ] !! ((n * (indexOfPerm (n - 1) xs') + j + 1) mod (n * fact (n - 1)))
~~~

The value `j` is the index of `n` in `xs`, so that `0 < j < n`. The first case is `j < n - 1`:

~~~
-- nextPerm n xs
--    { Assume j < n - 1 }
--     = insertAt (j + 1) n (perms (n - 1) !! (indexOfPerm (n - 1) xs'))
--     { By earlier assumption }
--     = insertAt (j + 1) n xs'
~~~

The second case is when `j = n - 1`:

~~~
-- nextPerm n xs
--    { Assume j = n - 1 }
--     = [ insertAt i n p | p <- perms (n - 1), i <- [0..n-1] ] !! ((n * (1 + indexOfPerm (n - 1) xs')) mod (n * fact (n - 1)))
--     { By earlier assumption }
--     = insertAt 0 n (perms (n - 1) !! (1 + indexOfPerm (n - 1) xs'))
--     { Definition of nextPerm }
--     = insertAt 0 n (nextPerm (n - 1) xs')
~~~

Thus we arrive at our final definition of `nextPerm`:

~~~{.haskell}
nextPerm 0 [] = []
nextPerm n xs | j == n - 1  = insertAt 0 n (nextPerm (n - 1) xs')
              | otherwise   = insertAt (j + 1) n xs'
  where (j, xs') = extract n xs
~~~

## A Better Data Structure

The inverse `indexOfPerm` is only a partial function, because `perms n` returns a collection of lists of size `n`. In addition, the types of lists does not enforce the invariant that each element `perms n` is a permutation of `[1..n]`.

Using the `-XPolyKinds` GHC extension, we can express a type of permutations, indexed by size, allowing us to strengthen the type of `nextPerm`, specifying that `nextPerm` preserves the size of a permutation.

The following type definition will be lifted to the kind level, generating two constructors `Z :: Nat` and `S :: Nat -> Nat`

~~~{.haskell}
data Nat = Z | S Nat
_1 = S Z
_2 = S $ S Z
_3 = S $ S $ S Z
_4 = S $ S $ S $ S Z

showNat :: Nat -> String
showNat n = show (show' n 0) where
  show' :: Nat -> Int -> Int
  show' Z m = m
  show' (S n) m = show' n (m + 1)

instance Show Nat where
  show = showNat
~~~

The type `Leq n` of natural numbers less than or equal to `n`. The type is parameterised over the kind `Nat`.

~~~{.haskell}
data Leq :: Nat -> * where
  LeqZero :: Leq n
  LeqSucc :: Leq n -> Leq (S n)
~~~

We can embed numbers less than or equal to `n` into numbers less than or equal to `n + 1` for every `n`:

~~~{.haskell}
embed :: Leq n -> Leq (S n)
embed LeqZero = LeqZero
embed (LeqSucc n) = LeqSucc (embed n)
~~~

We can convert to and from regular integers:

~~~{.haskell}
leqToInt :: Leq n -> Int
leqToInt LeqZero = 0
leqToInt (LeqSucc n) = 1 + leqToInt n

intToLeq :: Int -> EqNat n -> Leq n
intToLeq 0 n = LeqZero
intToLeq n (EqSucc m) = LeqSucc (intToLeq (n - 1) m)

showLeq :: Leq n -> String
showLeq n = show (show' n 0) where
  show' :: Leq n -> Int -> Int
  show' LeqZero m = m
  show' (LeqSucc n) m = show' n (m + 1)

instance Show (Leq n) where
  show = showLeq
~~~

The type of natural numbers equal to `n`, that is, a singleton type for each natural number:

~~~{.haskell}
data EqNat :: Nat -> * where
  EqZero :: EqNat Z
  EqSucc :: EqNat n -> EqNat (S n)

eq1 = EqSucc EqZero
eq2 = EqSucc $ EqSucc EqZero
eq3 = EqSucc $ EqSucc $ EqSucc EqZero
eq4 = EqSucc $ EqSucc $ EqSucc $ EqSucc EqZero
~~~

We can convert the sole inhabitant of each singleton type to its natural number representation:

~~~{.haskell}
eqToInt :: EqNat n -> Int
eqToInt EqZero = 0
eqToInt (EqSucc n) = 1 + eqToInt n

showEq :: EqNat n -> String
showEq n = show (show' n 0) where
  show' :: EqNat n -> Int -> Int
  show' EqZero m = m
  show' (EqSucc n) m = show' n (m + 1)

instance Show (EqNat n) where
  show = showEq
~~~

We will need the following helper method, which returns the value of `n` in the type of numbers less than or equal to `n`:

~~~{.haskell}
maxLeq :: EqNat n -> Leq n
maxLeq EqZero = LeqZero
maxLeq (EqSucc n) = LeqSucc (maxLeq n)
~~~

We can turn collect the list of all numbers in `Leq n` recursively:

~~~{.haskell}
for :: EqNat n -> [Leq n]
for EqZero = [LeqZero]
for (EqSucc n) = LeqZero : map LeqSucc (for n)
~~~

Finally, we define the type of permutations, again parameterised by the kind `Nat` and containing two type constructors: the empty permutation and the permutation obtained by inserting the value `n + 1` into a permutation of the list `[1..n]`:

~~~{.haskell}
data Perm :: Nat -> * where
  Empty :: Perm Z
  Insert :: Leq n -> Perm n -> Perm (S n)

showPerm :: Perm n -> String
showPerm p = "(" ++ concat (intersperse "," (map show (toList p 0))) ++ ")" where
  toList :: Perm n -> Int -> [Int]
  toList Empty m = []
  toList (Insert n p) m = let (l, r) = splitAt (leqToInt n) (toList p (m + 1)) in l ++ [m] ++ r
                         
instance Show (Perm n) where
  show = showPerm
~~~

Note now that invalid permutations are no longer inhabitants of the type `Perm n` for any `n`: to insert value `n` into a permutation of `[1..n-1]`, we have to specify a position to insert which is in the range `[0..n]`, and this is enforced by the type `Perm n`! One cannot, for example, represent a list with a duplicate element - the elements are not even mentioned explicitly.

The rank of a permutation is the size of the set it permutes:

~~~{.haskell}
rank :: Perm n -> EqNat n
rank Empty = EqZero
rank (Insert n p) = EqSucc (rank p)
~~~

The identity permutation is easily defined by recursion:

~~~{.haskell}
identity :: EqNat n -> Perm n
identity EqZero = Empty
identity (EqSucc n) = Insert LeqZero (identity n)
~~~

The method `perms` translates easily to this new setting:

~~~{.haskell}
perms1 :: EqNat n -> [Perm n]
perms1 EqZero = [Empty]
perms1 (EqSucc n) = [ Insert i xs | xs <- perms1 n, i <- for n ]
~~~

We can create a permutation from its list representation by repeatedly extracting the highest element:

~~~{.haskell}
fromList :: EqNat n -> [Int] -> Perm n
fromList EqZero [] = Empty
fromList (EqSucc n) xs = Insert (intToLeq i n) (fromList n (map (flip (-) 1) (delete 0 xs)))
  where Just i = elemIndex 0 xs
~~~

We can also translate the function `indexOfPerm` without difficulty:

~~~{.haskell}
indexOfPerm1 :: Perm n -> EqNat n -> Int
indexOfPerm1 Empty EqZero = 0
indexOfPerm1 (Insert n p) (EqSucc m) = (indexOfPerm1 p m) * (1 + eqToInt m) + leqToInt n
~~~

The following function emulates the indexing function `perms r !!`, returning the `n`th permutation in the set of permutations of a given rank:

~~~{.haskell}
nth :: Int -> EqNat n -> Perm n
nth 0 EqZero = Empty
nth m (EqSucc n) = Insert (intToLeq k n) (nth j n)
  where (j, k) = divMod m (1 + eqToInt n)
~~~

As before, we can combine `nth` with `indexOfPerm1` to step to the next permutation:

~~~{.haskell}
nextPerm1' :: Perm n -> Perm n
nextPerm1' p = let r = rank p in nth (indexOfPerm1 p r - 1) r
~~~

Finally, we can perform the same fusion as before, and express `nextPerm1` directly without the need for helper functions `nth` and `indexOfPerm1`.

~~~{.haskell}
nextPerm1 :: Perm n -> Perm n
nextPerm1 Empty = Empty
nextPerm1 (Insert LeqZero p) = Insert (maxLeq (rank p)) (nextPerm1 p)
nextPerm1 (Insert (LeqSucc n) p) = Insert (embed n) p
~~~

Note here that we have also removed the dependence on the intermediate type `Int`, representing the index of the permutation, and we are left with a type which conveys some valuable information about the function `nextPerm1`:

~~~
-- nextPerm1 :: forall (n :: Nat). Perm n -> Perm n
~~~

That is, `nextPerm1` preserves the rank of its argument.

Compile the source in this post with GHC 7.4 or later.

