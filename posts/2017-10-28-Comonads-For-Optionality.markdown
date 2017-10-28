---
title: Comonads for Optionality
author: Phil Freeman
date: 2017/10/28
description:
tags: Haskell
---

### Motivation

I've been thinking more about [comonads as spaces](http://blog.functorial.com/posts/2016-08-07-Comonads-As-Spaces.html) of user interface states lately. You might like to read that blog post before continuing here, if you haven't already.

Based on the notes in some of my recent blog posts, we have comonads for describing almost all useful compositions of user interfaces:

- Independent adjacent components, using [Day convolution](http://blog.functorial.com/posts/2016-08-08-Comonad-And-Day-Convolution.html)
- Static lists of components, using [the free Applicative functor](http://blog.functorial.com/posts/2017-07-01-FreeAp-Is-A-Comonad.html)
- Components with shared state, constructed using [equalizers of comonads](http://blog.functorial.com/posts/2017-07-13-Equalizers-of-Comonads.html)

This approach certainly seems to be a useful source of new comonads! But there is one sort of component which is notably missing from this list - components for _optional data_.

Consider the `Coproduct` comonad. This allows us to represent components with two possible classes of states, which is useful. But the implementation of `duplicate` means that we cannot move _between_ those two classes. There is no function that we can `extend` over a `Coproduct` in order to move from the left to the right, or back again.

Compare this to a component for a list of subcomponents which is built from the free applicative. Since
the free applicative is essentially built from coproducts and Day convolution, we
end up being able to manipulate the state of each list, but unable to change the _length_ of the list or permute its elements.

In this post, I'll show how we can create a `Comonad` to recover this sort of functionality.

### Inspiration

Let's consider the `Store` comonad. This comonad certainly allows us a lot of freedom - we can move freely to any other state we want!

To see why we are able to change the state `s`, we can look at the `Extend` instance:

```purescript
data Store s a = Store s (s -> a)

instance extendStore :: Extend (Store s) where
  extend f (Store s get) = Store s \next -> f (Store next get)
```

Notice that `f` receives the store refocussed at the `next` state.

We can take this as inspiration for our new comonad.

### Comonads for optionality

Let's start by creating a comonad with two possible states: empty and non-empty. The possible states in the non-empty case will be represented by some other comonad `w`.

We could start with a `Store` with two possible states, represented by the `Boolean` type. But this wouldn't work, because the return type `a` needs to depend on the current boolean state. If the boolean is `true` (representing an empty state), we can return a value of type `a` (there is only one empty state, after all), but if the boolean is `false` (representing a non-empty state), then we need to return a value of type `w a` (representing all possible future states of the subcomponent).

So what we really need is some sort of "dependent store" comonad:

```purescript
data DependentStore index (w :: index -> * -> *) a = DependentStore index ((i :: index) -> w i a)
```

Here, the comonad `w` is parameterized by an index type, and the choice of comonad in the result depends on the new index that we receive.

Of course, we can't represent this type in Haskell directly, but fortunately, we don't need to in the case of a `Boolean`-valued index, since a dependent function from a `Boolean` can be represented directly as a product:

```purescript
data Optional w a = Optional Boolean a (w a)
```

The idea here is that the `a` argument here tracks the case where the incoming `Boolean` is `true`, and the `w a` tracks the case where it is `false`. With that in mind, we can write out our `Store`-inspired instances:

```purescript
instance extendOptional :: Extend w => Extend (Optional w) where
  extend f (Optional b a wa) =
    Optional b
             (f (Optional true a wa))
             (extend (f <<< Optional false a) wa)

instance comonadOptional :: Comonad w => Comonad (Optional w) where
  extract (Optional true a _) = a
  extract (Optional false _ wa) = extract wa
```

There are a couple of interesting things to note here:

- The `f` function passed to `extend` receives a refocussed comonad, whose state is `true` or `false` depending on if it is being used to redecorate the left-hand `a` or the right-hand `w a`. This is just like in the `Store` case above.
- The `extract` function behaves as if it were passing the current state into the (dependent) function, so it takes the left-hand `a` if the current state is `true`, or extracts from the right-hand `w a` if the state is `false`.

This comonad is enough to model user interfaces with optional subcomponents, such as tabs, but we can do more!

### Comonads for lists

A linked list is isomorphic to an optional pair of its head and its tail:

```purescript
List a ~ Maybe (Tuple a (List a))
```

Similarly, we can use our `Optional` comonad to build a comonad for lists of subcomponents:

```purescript
newtype ListOf w a = ListOf (Optional (Day w (ListOf w)) a)
```

In the definition of `ListOf`, we use `Day` convolution to represent the conjunction of the component modeling the head of the list, and the component modeling the tail.

This is a `Comonad` by construction. Since this is a `newtype`, we can even derive its instances:

```purescript
derive newtype instance extendListOf :: Extend w => Extend (ListOf w)
derive newtype instance comonadListOf :: Comonad w => Comonad (ListOf w)
```

The `ListOf w a` type represents an infinite structure, with alternatives for every possible future length of our list. We can `extend` a function across this structure which changes the length to any new length we want.

### Another approach to lists

In fact, there is another way to construct a component for lists - we can consider a list to be indexed by its length, and use our dependent store comonad:

```purescript
data ListOf' w a = ListOf' Nat ((n :: Nat) -> FreeAp n w a)
```

(again, of course, this type is not representable directly in Haskell or PureScript)

Here, `FreeAp` is an indexed free applicative functor, indexed by the number of copies of `w` that it contains:

```purescript
data FreeAp (n :: Nat) w a where
  Nil :: Identity a -> FreeAp 0 w a
  Cons :: Day w (FreeAp n w) a -> FreeAp (n + 1) w a
```

So we can think of our dependent store comonad as recovering some of the dynamic behavior that we lost when we modeled our list using the free applicative.

### Conclusion

This construction fills in the remaining gap in the comonads as spaces approach to user interfaces, and suggests a new interesting way to build new comonads from old. We can construct models for optional subcomponents, and as we've seen, for lists of subcomponents.

I'd like to thank Arthur Xavier for providing valuable feedback and testing on the ideas presented here.

### Appendix: Proofs of the comonad laws

#### `extract . duplicate = id`

```purescript
extract (duplicate e@(Optional b a wa))
= if b then (Optional true a wa) else extract (map (Optional false a) (duplicate wa))
= if b then e else Optional false a wa
= if b then e else e
= e
```

#### `map extract . duplicate = id`

```purescript
map extract (duplicate e@(Optional b a wa))
= map extract (Optional b (Optional true a wa) (map (Optional false a) (duplicate wa)))
= Optional b (extract (Optional true a wa)) (map extract (map (Optional false a) (duplicate wa))))
= Optional b a (map (extract <<< Optional false a) (duplicate wa)))
= Optional b a (map extract (duplicate wa)))
= Optional b a wa
= e
```

#### `duplicate . duplicate = map duplicate . duplicate`

```purescript
duplicate (duplicate e@(Optional b a wa))
= duplicate (Optional b (Optional true a wa) (map (Optional false a) (duplicate wa)))
= Optional b (Optional true (Optional true a wa) (map (Optional false a) (duplicate wa))) (map (Optional false (Optional true a wa)) (duplicate (map (Optional false a) (duplicate wa))))
= Optional b (duplicate (Optional true a wa)) (map (Optional false (Optional true a wa)) (duplicate (map (Optional false a) (duplicate wa))))
= ... (see lemma below)
= Optional b (duplicate (Optional true a wa)) (map duplicate (map (Optional false a) (duplicate wa)))
= map duplicate (Optional b (Optional true a wa) (map (Optional false a) (duplicate wa)))
= map duplicate (duplicate e@(Optional b a wa))
```

#### Proof of Lemma

```purescript
map (Optional false (Optional true a wa)) (duplicate (map (Optional false a) (duplicate wa)))
= map (Optional false (Optional true a wa)) (map (map (Optional false a)) (duplicate (duplicate wa)))
= map (Optional false (Optional true a wa) <<< map (Optional false a)) (map duplicate (duplicate wa))
= map (\w -> Optional false (Optional true a wa) (map (Optional false a) w)) (map duplicate (duplicate wa))
= map (\w -> Optional false (Optional true a wa) (map (Optional false a) (duplicate w))) (duplicate wa)
= map (\w -> duplicate (Optional false a w)) (duplicate wa)
= map duplicate (map (Optional false a) (duplicate wa))
```
