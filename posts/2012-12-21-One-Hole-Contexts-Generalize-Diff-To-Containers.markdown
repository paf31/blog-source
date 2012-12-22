---
title: One-Hole Contexts Generalize Diff To Containers
author: Phil Freeman
date: 2012/12/21
description: A generalization of the usual diff/patch functions on lists to arbitrary container types
tags: Haskell
---

Text-based diff is of limited usefulness for analysing changes to a large code base over time. I've wondered for a while how one might generalize the `diff` and `patch` functions on lists to more general data structures such as abstract syntax trees.

It's pretty easy to write down a version of the `diff` function for, say, binary trees, but less simple to write a function which works generically across multiple data types. As usual, I spent a while thinking about this before realizing the problem had already been solved ([1], [2]). I'm still quite happy with the approach I came up with, and I think it's sufficiently interesting to write about here anyway.

~~~{.haskell}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Diff where

import Data.Function (on)
import Data.List (maximumBy)
~~~

## Longest Common Subsequences and Largest Common Substructures

The `diff` function can be specified as follows: for lists `xs` and `ys`, find the shortest edit sequence taking `xs` to `ys`. We can reduce the problem to finding the longest common subsequence of `xs` and `ys`: to obtain the least common subsequence, first remove a subset of elements from `xs`, and then to obtain `ys`, insert a subset of elements of `ys` into the least common subsequence.

Ignoring optimization for the time being, let's take this as our starting point. We'll generalize the longest common subsequence to the largest common substructure.

First, let's examine the inductive definition of all subsequences of a list:

~~~{.haskell}
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = [ xs'' | xs' <- subsequences xs, xs'' <- [ xs', x:xs' ] ]
~~~

In words: if the input list has no elements, return just the input, otherwise, for each subsequence of the tail, yield two lists: the first including the head of the input, and the second excluding it.

Let's recast that definition in the more generic language of containers: 

If the input has no subcontainers, return just the input, otherwise, for each substructure of each subcontainer, yield two substructures: the first including the structure of the input, and the second including only the substructure.

Let's define a type class:

~~~{.haskell}
newtype Rec f = Rec { runRec :: f (Rec f) }

class Container f where
  data Context f :: * -> *
  children :: f a -> [(a, Context f a)]
  plugIn :: a -> Context f a -> f a
  childAt :: f a -> Context f a -> a
~~~

The type `Rec f` is the usual type of recursive data structures of shape `f`. The `Container` class contains a few methods which deserve explanation.

The associated data type `Context f` is the type of one-hole contexts of `f` [3]. The `children` function takes an input of type `f a` and returns an array of the contained `a`'s along with their one-hole contexts. The `plugIn` method takes an `a` and a context, and plugs the `a` into the hole defined by context. Finally, the `childAt` function takes an input of type `f a` and a context, and returns the `a` in the input at that context.

There are some fairly obvious laws which instances of the `Container` class should satisfy:

    childAt (plugIn a c) c = a
    plugIn (childAt a c) c = a
    (childAt a c) `elem` (children a)

Let's define the type of changes for structures of shape `f`:

~~~{.haskell}
type Step f = Context f (Rec f)

data Change f = Skip (Step f) | Take (Step f) | Replace (Rec f) (Rec f)

type Path f = [Change f]
~~~

and derive some instances:

~~~{.haskell}
deriving instance (Eq (f (Rec f))) => Eq (Rec f)

deriving instance (Show (f (Rec f))) => Show (Rec f)

deriving instance (Show (Context f (Rec f)), Show (Rec f)) => Show (Change f)
~~~

With that, let's define the generalization of `subsequences`:

~~~{.haskell}
type InContext f = (Rec f, Path f)

substructures :: (Container f) => Rec f -> [InContext f]
substructures (Rec x) 
  | null $ children x = [(Rec x, [])]
  | otherwise = 
    [ substructure
    | (x', ctx) <- children x,
      (x'', ctxs) <- substructures x',
      substructure <- [ (x'', Skip ctx:ctxs), (Rec $ plugIn x'' ctx, Take ctx:ctxs) ] ]
~~~

Here, the `plugIn` and `children` methods are used to generalize the inclusion/exclusion which took place when we calculated subsequences of a list.

We can now easily find the largest common substructure, and therefore the difference of two structures.

~~~{.haskell}
type ChangeSet f = (Path f, Path f)

common :: (Container f, Eq (Rec f)) => [InContext f] -> [InContext f] -> [ChangeSet f]
common xs ys = [ (p1, p2) | (s1, p1) <- xs, (s2, p2) <- ys, s1 == s2 ]

takes :: Path f -> Path f
takes = filter take where
  take (Take _) = True
  take _ = False

size :: ChangeSet f -> Int
size (xs, ys) = length (takes xs) + length (takes ys)

largest :: [ChangeSet f] -> ChangeSet f
largest = maximumBy (compare `on` size)

diff :: (Container f, Eq (Rec f)) => Rec f -> Rec f -> ChangeSet f
diff old new = largest $ (common `on` substructures) old new
~~~

## Example 1 - Lists

Let's verify the code on the simple case of lists.

~~~{.haskell}
data ListF a x = Nil | Cons a x deriving (Show, Eq)

type List a = Rec (ListF a)

instance Container (ListF a) where
  data Context (ListF a) x = ListContext a deriving (Show, Eq)
  children Nil = []
  children (Cons a x) = [(x, ListContext a)]
  plugIn x (ListContext a) = Cons a x
  childAt (Cons _ x) (ListContext _) = x

nil :: List a
nil = Rec Nil

cons :: a -> List a -> List a
cons a x = Rec $ Cons a x

fromList :: [a] -> List a
fromList = foldr cons nil
~~~

In GHCi:

    > diff (fromList [1,2,3]) (fromList [2,3,4])
    ([Skip (ListContext 1), Take (ListContext 2), Take (ListContext 3)],
     [Take (ListContext 2), Take (ListContext 3), Skip (ListContext 4)])

The result is as expected: from the first list we skip the first element and take the rest, and from the second list we skip the last element and take the rest.

## Example 2 : Lambda Terms

Now here's a more exciting example - the type of untyped lambda terms:

~~~{.haskell}
data ExprF x = App x x | Abst String x | Var String deriving (Show, Eq)

type Expr a = Rec ExprF

instance Container ExprF where
  data Context ExprF x = AppContext (Either x x) | AbstContext String deriving (Show, Eq)
  children (App f x) = [(f, AppContext $ Right x), (x, AppContext $ Left f)]
  children (Abst i x) = [(x, AbstContext i)]
  children (Var _) = []
  plugIn x (AppContext (Left f)) = App f x
  plugIn f (AppContext (Right x)) = App f x
  plugIn x (AbstContext i) = Abst i x
  childAt (App _ x) (AppContext (Left _)) = x
  childAt (App f _) (AppContext (Right _)) = f
  childAt (Abst _ x) (AbstContext _) = x

abst :: Int -> Expr -> Expr
abst i x = Rec $ Abst i x

app :: Expr -> Expr -> Expr
app f x = Rec $ App f x

var :: Int -> Expr
var i = Rec $ Var i

k :: Expr
k = abst "x" $ abst "y" $ var "x"

s :: Expr
s = abst "x" $ abst "y" $ abst "z" $ app (app (var "x") (var "z")) (app (var "y") (var "z"))
~~~

And again, we can test in GHCi:

    diff s k
    > ([Take (AbstContext "x"), Take (AbstContext "y"), 
        Skip (AbstContext "z"), Skip (AppContext (Right (Rec {runRec = App (Rec { runRec = Var "y" }) ... }))), 
        Skip (AppContext (Right (Rec { runRec = Var "z" })))],
       [Take (AbstContext "x"), Take (AbstContext "y")])
       
The diff algorithm has identified `k` as the largest common substructure, and the path through `s` which picks out this substructure discards the third abstraction over the variable `z` and the corresponding applications.

## Applying Patches

We can also define a function `patch` which applies the result of `diff` to a structure:

~~~{.haskell}
patch :: (Container f, Eq (Rec f)) => Rec f -> ChangeSet f -> Rec f
patch old (inserts, deletes) = foldr wrap (unwrap old inserts) deletes where
  wrap (Skip _) x = x
  wrap (Take ctx) x = Rec $ plugIn x ctx
  wrap (Replace x y) _ = y
  unwrap x [] = x
  unwrap x (Skip ctx:cs) = Rec $ plugIn (unwrap (childAt (runRec x) ctx) cs) ctx
  unwrap x (Take ctx:cs) = unwrap (childAt (runRec x) ctx) cs
  unwrap _ (Replace x y:cs) = unwrap x cs
~~~

The functions `plugIn` and `childAt` play a key role, since we need to be able to glue structures together and take them apart again based on the content of the changeset.

We can verify using the examples above (and others) that `diff` and `patch` satisfy the required laws `patch x $ diff x y = y` and `diff x $ patch x p = p`.

## Optimization

The function `diff` above does its job, but has exponential complexity in the size of its input, due to the need to generate all substructures.

The key observation is that if two substructures are unequal, then two larger structures can never be equal, so we can prune large subtrees from the search tree by testing equality of contexts.

Here is the optimized version of `diff`, which is observably much faster than the original:

~~~{.haskell}
diff2 :: (Container f, Eq (Context f (Rec f))) => Rec f -> Rec f -> ChangeSet f
diff2 old new 
  | null $ children $ runRec old = ([], [Replace old new])
  | null $ children $ runRec new = ([Replace old new], [])
  | otherwise = let matches = [ (x', y', ctx1)  
                              | (x', ctx1) <- children $ runRec old 
                              , (y', ctx2) <- children $ runRec new
                              , ctx1 == ctx2 ] in
                if null matches then
                  largest $ [ let (xs, ys) = diff2 x' new in (Skip ctx:xs, ys)
                            | (x',ctx) <- children $ runRec old ] ++
                            [ let (xs, ys) = diff2 old y' in (xs, Skip ctx:ys)
                            | (y',ctx) <- children $ runRec new ]
                else
                  largest [ let (xs, ys) = diff2 x' y' in (Take ctx:xs, Take ctx:ys) 
                          | (x', y', ctx) <- matches ]
~~~

Notice that the new constraints only require equality of contexts, not of structures.

Compare this implementation with the dynamic programming implementation of the longest common subsequence on lists:

~~~{.haskell}
diffList :: (Eq a) => [a] -> [a] -> [a]
diffList [] new = []
diffList old [] = []
diffList old@(x:xs) new@(y:ys) 
  | x == y = (x:diffList xs ys)
  | otherwise = maximumBy (compare `on` length) [ diff old ys, diff xs new ]
~~~

Hopefully the structural similarity of the two algorithms is clear.

## Conclusion

One-hole contexts give a pleasant generalization of `diff` and `patch` to containers.

There are still some issues remaining with this implementation, such as the lack of memoization and the incorrect handling of containers with multiple non-recursive constructors, or structures with no common substructures.

It would also be interesting to explore the extension to mutually recursive types, such as the types of statements and expressions in an abstract syntax tree.

## References

1. Package Data.Generic.Diff on Hackage
2. Generic type-safe diff and patch for families of datatypes by Eelco Lempsick, 2009
3. The Derivative of a Regular Type is its Type of One-Hole Contexts by Conor McBride
