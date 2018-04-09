---
title: Incrementally Improving The DOM
author: Phil Freeman
date: 2018/04/08
description:
tags: Haskell
---

## Introduction

[Last time](http://blog.functorial.com/posts/2018-03-12-You-Might-Not-Need-The-Virtual-DOM.html), I tried to convince you that you might not need the virtual DOM, and that many common UI patterns can be reproduced with a completely _static_ page, with changes only happening at the leaves of the tree - attributes and text nodes. For some trickier UI patterns, I added back a limited form of dynamic behavior, by allowing elements with dynamic lists of children.

It is perhaps not terribly surprising that this is possible, since it is, after all, what we used to do before React popularized the virtual DOM (using things like Mustache templates).

The static DOM approach has some limitations of its own, however:

- Dynamic arrays are optimized for modification at the _end_ of the array. Modifications in the middle of an array can trigger a cascade of updates to nodes at the end of the array. In practice, this is not a big problem, but for large arrays it can become a performance issue. One solution to this problem is to create an alternative structure for looping, where the inner template does either not have access to its index, or where the indices do not correspond to the position of the element in the parent array.
- In order to trigger a UI change, however small, we need to construct a new model for the entire static DOM component. Again, in practice, this is not a big problem, but it does make it harder to do certain things. For example, if we wanted to send model changes to the server for evaluation, we would have a hard time.
- Every change is potentially observed by every node in the static DOM. We can use tricks like filtering out duplicate events from our event streams, but this takes unnecessary time and CPU cycles. Recall, the motivation for the static DOM was that we intuitively _knew_ which elements should receive the events for small model changes such as changing a single text node. The challenge is to convince the machine that this connection between submodels and elements is obvious!

In this post, I'd like to suggest a different approach, which solves these problems but keeps the benefits of the static DOM approach.

## Enter the Incremental Lambda Calculus

The paper ["A Theory of Changes for Higher-Order Languages"](https://arxiv.org/abs/1312.0658) by Cai, Giarrusso, Rendel and Ostermann states the following in its abstract:

> If the result of an expensive computation is invalidated by a small change to the input, the old result should be updated incrementally instead of reexecuting the whole computation.

This sounds a lot like it applies to our problem! Once we've computed the initial state of the DOM, a small change to the model should result in a small change to the DOM.

In fact, as we'll see, the incremental lambda calculus will provide a solution to all three of the problems listed above.

"A Theory of Changes..." proceeds by interpreting the types and terms of the lambda calculus in a new context where each type is augmented with a _change structure_.

For our purposes, a change structure is equivalent to a monoid acting on the values of the type. I implement change structures using the following type class:

```purescript
class Monoid m <= Patch a m | a -> m where
  patch :: a -> m -> a
```

This declaration states that there is a functional relationship between carrier types `a` and change structures `m`, which must be `Monoid`s. I use a functional dependency to express the change structure as a function of the carrier type. In practice, this means using newtypes in quite a few more places, but makes type inference more pleasant.

For example, the `Last a` monoid acts on values of the type `a` via the newtype `Atomic a`:

```purescript
import Data.Maybe.Last

newtype Atomic a = Atomic a

instance patchAtomic :: Patch (Atomic a) (Last a) where
  patch (Atomic a) (Last m) =
    case m of
      Nothing _ -> Atomic a
      Just b    -> Atomic b
```

`mempty` does nothing, keeping the current value, and when composing several `Last a` values, the last one wins. `Atomic a` is a value of type `a` with a trivial change structure, where the value is either not changed at all, or changed completely.

The paper also defines change structures for tuples (in which the two components can change independently), functions, and other structures such as _bags_ (sets with duplicate elements permitted).

By interpreting each type and term former in this context, the paper is able to interpret any term of the simply-typed lambda calculus as as _incremental_ function. An incremental function is one which can either be evaluated normally, or given a change to the input, can produce a change to the output.

## An Embedded DSL

In my [`purescript-incremental-functions`](https://github.com/paf31/purescript-incremental-functions) library, I use a different approach, keeping the change structure concept, but implementing incremental functions using an _embedded DSL_. In particular, I use an approach based on _higher-order abstract syntax_, in which incremental functions are represented using regular PureScript functions.

It should perhaps not be surprising (if you've read my [other blog post](http://blog.functorial.com/posts/2017-10-08-HOAS-CCCs.html), anyway) that it is possible to give an embedding of incremental lambda calculus in terms of higher-order abstract syntax, but the embedding I use here is in fact _not_ the one I describe in that blog post - it is much simpler.

The key data structure we'll need is a `Jet`:

```purescript
type Jet a =
  { position :: a
  , velocity :: Change a
  }
```

A `Jet` is a value of type `a`, paired with a change of type `Change a`, where `Change a` is the change structure acting on `a`. I say "_the_ change structure", since the functional dependency on `Patch` makes it unique.

`Change` is defined using something like an _associated type_. In PureScript, unlike in GHC Haskell, we don't have associated types, but we can make a crude approximation by packaging up the (unique) type under a fundep as an abstract data type and using `unsafeCoerce` to construct values (safely!):

```purescript
data Change a

fromChange :: forall a da. Patch a da => Change a -> da
fromChange = unsafeCoerce

toChange :: forall a da. Patch a da => da -> Change a
toChange = unsafeCoerce
```

We should think of the value `Jet { position: x, velocity: dx }` as being positioned currently at `x`, and _about to move_ by the amount `dx`. This might be reminiscent of dual numbers, from automatic differentiation, in which we pair a number with its rate of change.

Given the definition of `Jet`, the encoding of incremental functions is simple: an incremental function from `a` to `b` (with their associated change structures) is represented by a function from `Jet a` to `Jet b`.

Here is a simple example - an incremental function from `Atomic` values of type `a` to `Atomic` values of type `b`, constructed from a regular function from `a` to `b`:

```purescript
mapAtomic :: forall a b. (a -> b) -> Jet (Atomic a) -> Jet (Atomic b)
mapAtomic f { position, velocity } =
  { position: Atomic (f (un Atomic position))
  , velocity: toChange (map f (fromChange velocity))
  }
```

Here, the result will change only when the input changes.

This is a simple example, but we can create incremental versions of many standard functions: maps, folds, `filter`, `zip`, and so on. `purescript-incremental-functions` defines a small standard library of incremental data structures such as arrays, maps and records, and incremental functions like these.

To illustrate an important point, here is another example - an API for an incremental map data structure and a function to `map` a function over it:

```purescript
data IMap k a

data MapChange a da
  = Insert a
  | Remove
  | Update da

type MapChanges k a da = Map k (MapChange a da)
-- ^ a potential change for each key

map
  :: forall k a da b db
   . Ord k
  => Patch a da
  => Patch b db
  => (Jet a -> Jet b)
  -> Jet (IMap k a)
  -> Jet (IMap k b)
```

Note that jet functions are used here to construct a _higher-order incremental function_, since the (incremental) function being mapped is being passed in as an argument.

Since jet functions are just regular functions, we can compose them like functions, use lambda abstraction to form new functions, and so on. We are propagating changes by passing them from one function to the next. For example:

```purescript
mapAtomic (_ + 1)
  :: Jet Int -> Jet Int

map (mapAtomic (_ + 1))
  :: Jet (Imap k Int)
  -> Jet (IMap k Int)

\f -> map (map f)
  :: (Jet a -> Jet b)
  -> Jet (IMap k1 (IMap k2 a))
  -> Jet (IMap k1 (IMap k2 b))
```

If we squint enough to see through the `Jet` type constructors, this DSL is very close to plain old functions, but where our data structures have been switched out for their incremental equivalents. Of course, we're limited to those basic functions which we can write incrementally as jet functions.

## Laws for Incremental Functions

Every incremental function can be represented as a jet function, but not every jet function is a valid incremental function. We require the following condition to hold for a jet function `f :: Jet a -> Jet b`:

```purescript
patch (lower f a) db = lower f (patch a da)
```

where

```purescript
lower :: (Jet a -> Jet b) -> a -> b
lower f a = (f { position: a, velocity: mempty }).position
```

and

```purescript
db = (f { position: a, velocity: da }).velocity
```

That is, if we `lower` the function `f` to a function on regular (non-changing) values, and apply it to a `patch`ed value, we should get the same result as applying the `lower`ed function, and patching the result with a patch generated by the jet function.

Note, however, that the following condition, which might seem intuitively obvious, _does not_ hold in general:

```purescript
(f { position: a, velocity: mempty }).velocity == mempty
```

That is, jet functions are not required to take constant jets to constant jets. The reason is that a jet function might close over some already-changing value from its environment, in which case changes in the result would already be "baked in" to that jet function.

## Change Structures as Models for Mutation

Change structures tell us how changes act on values, as pure functions, but we can use those pure functions to model impure changes to the real world.

For example, the paper talks about how incremental lambda calculus could be used to model _self-maintaining database views_. In this case, our values would represent _relations_ and changes would represent updates on those relations. By propagating changes from simple relations to computed relations, we can hopefully optimize the way in which our views get maintained. This is the sort of thing that database developers implement in triggers all the time, but wouldn't it be great to be able to _derive_ updates from the definition of the view itself?

However, as I've hinted already, I'm interested in a different application of this idea - to incrementally updating the DOM.

Generally, this suggests a different way to deal with imperative, mutation-heavy APIs - first, model the changes we intend to apply via that API, and model them purely using a change structure for some conceptual representation of the domain. Next, write an interpreter which can interpret that change structure, and finally use the incremental lambda calculus to implement the wiring from the changes in our simplified model to changes in the real world.

## An Incremental Model of the DOM

Let's see this approach in action, applied to the DOM.

Here is a simple data structure which models DOM elements, like we might find in a virtual DOM library:

```purescript
newtype View = View
  { element  :: String
  , text     :: Atomic String
  , attrs    :: IMap String (Atomic String)
  , handlers :: IMap String (Atomic EventListener)
  , kids     :: IArray View
  }
```

A `View` consists of an element name (such as `div`, `img` and so on), some text content, a map of attributes, a map of event handlers and an array of children.

Now, here is a change structure for this type, which is derived naively in terms of the change structures of the types in the record labels:

```purescript
newtype ViewChanges = ViewChanges
  { text     :: Last String
  , attrs    :: MapChanges String (Atomic String) (Last String)
  , handlers :: MapChanges String (Atomic EventListener) (Last EventListener)
  , kids     :: Array (ArrayChange View ViewChanges)
  }

instance patchView :: Patch View ViewChanges
```

We can write incremental functions which construct these `View`s. For example:

```purescript
text :: Jet (Atomic String) -> Jet View

element
  :: String
  -> Jet (IMap String (Atomic String))
  -> Jet (IMap String (Atomic EventListener))
  -> Jet (IArray View)
  -> Jet View
```

And now, a basic application loop is easy enough to implement. A component can be described by a jet function of two arguments, a model, and a callback which responds to changes to the model:

```purescript
type Component model
    = Jet (Atomic (Change model -> EventListener))
   -> Jet model
   -> Jet View
```

To start, we run this function in regular mode, passing in our initial model to render an initial view. We then render that `View` to the DOM normally.

The function of type `Change model -> EventListener` takes changes generated by the view and applies them to the current model to obtain a change to the view. If we can write an interpreter which turns the `ViewChanges` change structure into _actual_ view changes, then we can update the DOM in response to user events.

This interpreter is what my [`purescript-purview`](https://github.com/paf31/purescript-purview) library provides. It is a barebones implementation of a change structure for the DOM which can be interpreted as _actual_ changes to the DOM. With this, we can implement a variety of different abstractions for building UIs which propagate incremental changes to the DOM.

The thing I like the most about this approach is that it really forces you think about the changes you plan to apply to your components before you start implementing them. In addition, these choices reflect the trade-offs in the various diffing algorithms which are implicit in some virtual DOM implementations. For example, "should I use an IMap or an IArray?" is similar to choosing between keyed and non-keyed implementations.

## Conclusion

Incremental lambda calculus solves the problems with the virtual DOM which I outlined in my previous blog post. Specifically, we keep the clear denotation that the virtual DOM gives us - our components are still just functions, but now _incremental_ functions - and we simplify the operational semantics by omitting the diffing step required by the virtual DOM, and instead propagate changes directly.

It also solves the problems with the "static DOM" approach that I listed above. Specifically:

- There is no inherent performance penalty associated with random access to incremental arrays.
- We don't need an event system at all, so there is no explosion of events.
- Most interestingly, we don't need to materialize an entire model in order to apply a small change. We only deal with changes, and those changes are plain old data structures which can be easily sent over the network, allowing us to decouple view logic from view rendering.

In practice, this approach takes some getting used to if you are already used to a virtual DOM approach, but the fact that we're dealing with plain old functions makes it simple enough to get started and build real components.
