---
title: Generalizing O(1) Snoc On Lists
author: Phil Freeman
date: 2012/06/17
description: A generalization of snoc to traversable data structures
tags: Haskell
---

I\'ve recently been thinking about how to generalize an efficient version of the `snoc` operation on lists.

Specifically, one can represent lists by identifying them with their fold functions:

~~~
-- data List a = List { fold :: forall r. r -> (a -> r -> r) -> r }
~~~

This representation, as in the case of difference lists, admits efficient versions of the cons operation at both ends of the list:

~~~
-- cons :: a -> List a -> List a
-- cons a l = List (\r0 acc -> acc a $ fold l r0 acc)

-- snoc :: a -List a -> List a
-- snoc a l = List (\r0 acc -> fold l (acc a r0) acc)
~~~

I wondered if it was possible to generialize this to arbitrary least fixed point types. What I came up with is satisfactory, but I wonder if there is a better generalization of the original `snoc` function.

We\'ll need these imports and extensions:

~~~{.haskell}
{-# LANGUAGE RankNTypes, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import qualified Data.Void as V
import qualified Data.Monoid as M
import qualified Data.Foldable as F
import qualified Data.Traversable as T
~~~

First, let\'s generalize the type `List` given above to any type constructor `f`:

~~~{.haskell}
newtype Rec f = Rec { cata :: forall r. (f r -> r) -> r }
~~~

The `cons` operation is easily generalized - `Rec f` is an initial `f`-algebra, and the algebra map gives the generalization of `cons`:

~~~{.haskell}
gcons :: (Functor f) => f (Rec f) -> Rec f
gcons x = Rec cata' where
  cata' phi = phi $ fmap (flip cata phi) x
~~~

Generalizing `snoc` is a little less obvious. I came up with the following.

What I\'d like to do is to replace the leaves of the recursive structure with one more level of `f`-structure. By "leaves" I mean values of `Rec f` with no structurally smaller elements. These are given by the summands in `(f x)` with no factor of `x`. In other words, the type of leaves of `f` is given by applying `f` to the empty type `Void`.

Given a `Traversable` instance for `f`, we can attempt to convert a value into a leaf by replacing all instances of the type `a` by `Nothing`, and then collapsing the structure to a single `Maybe` value using `mapM`:

~~~{.haskell}
toLeaf :: (Functor f, T.Traversable f) => f a -> Maybe (f V.Void)
toLeaf = T.mapM (const Nothing)
~~~

Now we can define a generalized version of the `snoc` method above. `gsnoc` takes a function which replaces leaves with an additional level of `f`-structure, and applies it only to the leaves:

~~~{.haskell}
gsnoc :: (Functor f, T.Traversable f) => (forall r. f V.Void -> f (f V.Void)) -> Rec f -> Rec f
gsnoc f x = Rec cata' where
  cata' phi = cata x (phi' phi)
  phi' phi y = case toLeaf y of 
    Nothing -> phi y
    (Just leaf) -> phi $ fmap (($) phi . fmap V.absurd) $ f leaf
~~~~

`gsnoc` is also easily seen to be an O(1) operation, independent of the size of its input.

Now let\'s see some examples. First, we'll recover the usual O(1) snoc operation on lists.

~~~{.haskell}
data ListF a x = Nil | Cons a x deriving (Functor, F.Foldable, T.Traversable)

type List a = Rec (ListF a)
~~~

We can define two constructor functions and a single destructor which will help when working with this representation of lists:

~~~{.haskell}
nil :: List a
nil = gcons Nil

cons :: a -> List a -> List a
cons a l = gcons $ Cons a l

toList :: List a -> [a]
toList = flip cata toList' where
  toList' Nil = []
  toList' (Cons a l) = a:l
~~~

We can create a list of three elements and append an element to its tail:

~~~{.haskell}
test1 :: List Int
test1 = cons 1 $ cons 2 $ cons 3 nil

test2 :: List Int
test2 = gsnoc (Cons 4) test1

-- toList test2
-- [1,2,3,4]
~~~

Now let\'s try a different recursive datatype - the type of binary trees:

~~~{.haskell}
data TreeF a x = Tip | Branch x a x deriving (Functor, F.Foldable, T.Traversable)

type Tree a = Rec (TreeF a)
~~~

Again, we can define helper methods to construct and destruct trees. This time, the method flattenTree with perform an in-order traversal of the tree to collapse the tree down to a single list:

~~~{.haskell}
tip :: Tree a
tip = gcons Tip

branch :: Tree a -> a -> Tree a -> Tree a
branch l a r = gcons $ Branch l a r

flattenTree :: Tree a -> [a]
flattenTree = flip cata flattenTree' where
  flattenTree' Tip = []
  flattenTree' (Branch l a r) = l ++ [a] ++ r
~~~

This time, `Tip` is the only type of leaf, and the `snoc` method will replace a `Tip` constructor with a `Branch`:

~~~{.haskell}
snocTree :: a -> r -> TreeF a r
snocTree a r = Branch r a r

test3 :: Tree Int
test3 = branch tip 1 (branch (branch tip 3 tip) 2 (branch tip 4 tip))

test4 :: Tree Int
test4 = gsnoc (snocTree 5) test3

-- flattenTree test4
-- [5,1,5,3,5,2,5,4,5]
~~~

Finally, we can also generalize the O(1) append method on lists, by replacing leaves with a whole new structure. Again, we require a `Traversable` instance as well as a `Functor` instance for `f`:

~~~{.haskell}
gappend :: (Functor f, T.Traversable f) => (f V.Void -> Rec f) -> Rec f -> Rec f
gappend f x = Rec cata' where
  cata' phi = cata x (phi' phi)
  phi' phi y = case toLeaf y of 
    Nothing -> phi y
    (Just leaf) -> cata (f leaf) phi
~~~

On lists, the generalized `append` function does what one would expect:

~~~
-- toList $ gappend (const test1) test1
-- [1,2,3,1,2,3]
~~~

On trees, `gappend` replaces each `Tip` with a new tree:

~~~
-- flattenTree $ gappend (const test3) test3
-- [1,3,2,4,1,1,3,2,4,3,1,3,2,4,2,1,3,2,4,4,1,3,2,4]
~~~
