---
title: Stack-Safe Traversals via Dissection
author: Phil Freeman
date: 2017/06/18
description:
tags: PureScript
---

Writing `Traversable` instances which are both efficient and safe can be quite tricky in a strict language like PureScript.
Consider writing an instance for a linked list, for example. The naive instance is not tail recursive, and will build up large
thunks for some `Applicative` instances:

```purescript
traverseList :: forall f a b. Applicative f => (a -> f b) -> List a -> f (List b)
traverseList f Nil = pure Nil
traverseList f (Cons x xs) = Cons <$> f x <*> traverseList f xs
```

As it turns out, we can make this traversal tail recursive by writing it as a fold (this is what the `purescript-lists` library does,
in fact). But that won't stop the instance from building up thunks, and it won't work for other functors. For example, how can we write a safe traversal on a binary tree without allocating an
intermediate list?

While I was thinking about this problem recently, I was reminded of the paper "Clowns to the left of me, jokers to the right" by
Conor McBride. The paper shows how to turn a fold over a data type into a step-by-step calculation backed by an explicit stack
of intermediate computations. Interestingly, the shape of the stack can be computed generically from the type of the input structure
by a process called _dissection_.

In PureScript, the type class for dissectible containers and their associated dissection (a `Bifunctor`) looks like this:

```purescript
class (Traversable f, Bifunctor d) <= Dissectible f d | f -> d where
  moveRight :: forall c j. Either (f j) (Tuple (d c j) c) -> Either (f c) (Tuple (d c j) j)
```

The `moveRight` structure allows us to move right inside the container, one step at a time, turning a "joker" on the right into
a "clown" on the left at each step.

We can write an instance for lists, or many other types of container (including trees):

```purescript
instance dissectibleList :: Dissectible List (Product (Clown List) (Joker List)) where
  moveRight (Left Nil) = Left Nil
  moveRight (Left (j : js)) = Right (Tuple (Product (Clown Nil) (Joker js)) j)
  moveRight (Right (Tuple (Product (Clown cs) (Joker js)) c)) =
    case js of
      Nil -> Left (reverse (c : cs))
      j : js' -> Right (Tuple (Product (Clown (c : cs)) (Joker js')) j)
```

As an example of turning a fold into a tail-recursive function, the paper shows how we can implement the `map` function from the
`Functor` type class from any container which supports dissection. In PureScript:

```purescript
mapP :: forall f d a b. Dissectible f d => (a -> b) -> f a -> f b
mapP f xs = tailRec step (Left xs) where
  step = moveRight >>> case _ of
           Left ys -> Done ys
           Right (Tuple dba a) -> Loop (Right (Tuple dba (f a)))
```

As I was reading, I noticed I could use the same idea to solve my original problem. Using `MonadRec`, we can get a
safe traversal for any dissectible container and any `MonadRec`, simply by switching from a regular tail recursion with
`tailRec` to a monadic tail recursion with `tailRecM`:

```purescript
traverseP :: forall m f d a b. Dissectible f d => MonadRec m => (a -> m b) -> f a -> m (f b)
traverseP f xs = tailRecM step (Left xs) where
  step = moveRight >>> case _ of
           Left ys -> pure (Done ys)
           Right (Tuple dba a) -> Loop <<< Right <<< Tuple dba <$> f a
```

Write a dissection, get a safe traversal for free!
