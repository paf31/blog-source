---
title: You Might Not Need The Virtual DOM
author: Phil Freeman
date: 2018/03/12
description: 
tags: Haskell
---

## Introduction

Lately, I've been working on a new library called [purescript-sdom](https://github.com/paf31/purescript-sdom). It is an attempt to build a UI library in 100% PureScript without using the virtual DOM. I'll give an overview of the motivation behind the library, and the way in which it was implemented.

## Motivation

The virtual DOM is a powerful technique for the development of web applications. Popularized by React, emulated by many other JavaScript libraries and ported to other languages, it has become commonplace in web development.

I think the main reason for this is that the idea is so straightforward - an application is essentially a function from states to the type of virtual DOM elements. Another reason is that the virtual DOM enables many other useful abstractions for building user interfaces. The Elm architecture is a great example of this, but there is [a whole category of VDOM-based abstractions](http://blog.functorial.com/posts/2016-08-07-Comonads-As-Spaces.html) waiting to be understood.

Denotationally speaking, the virtual DOM is very appealing, but operationally, it is a heavyweight solution for many problems. Every time we modify our application state and rerun our render function, we allocate a new tree of (virtual) DOM components, and perform a tree diff in order to discover what must change in the actual DOM. For many (most?) applications, this seems to be overkill. We can optimize the diff algorithm in various ways, but ultimately, the diffing is essentially unnecessary - for simple changes to the model, I know precisely which nodes need to be updated. We simply need a way to make this obvious in the code.

An example will help to illustrate the inefficiency here. Consider a form which contains text inputs and validation messages which appear when the user enters incorrect data. Each input corresponds directly to a validation message element, and so, when the model changes, we can update these elements directly. Without a virtual DOM, we would just lay out the form statically, set the initial attributes and add event handlers to update them. Crucially, the elements themselves would never change or move.

This is a deliberately simple example, but this approach works in many other cases. In general, we can lay out the initial UI once and use event handlers to update attributes as the model changes. We rarely need to modify or move the actual DOM elements.

You might say "but my data isn't always static - tables contain dynamic data from the database". This is correct, but in many cases, even lists of data can use this approach, since we have a (usually quite small) upper bound on the number of rows in a table, for example. We can simply allocate the elements up front, and hide the elements we don't need.

Of course, this approach has its limitations. We don't always have a small upper bound on the number of child elements - we might not have an upper bound at all. Even so, we can still take advantage of certain guarantees about the static nature of a UI, as we'll see.

## An Initial Encoding

Let's start with a simple data type that we might use as the basis of a virtual DOM implementation:

```purescript
data VDOM
  = Text String
  | Element
      String -- element name
      (Map AttributeName AttributeValue) -- attributes
      (Map EventName EventHandler) -- event handlers
      (Array VDOM) -- child elements
```

A `VDOM` node is either a `Text` node which contains a single string, or an `Element` with an element name, a collection of attributes, a collection of event handlers, and a collection of child elements.

In order do use this type to build a user interface, we would implement a function `model -> VDOM` from _models_ to our type of virtual DOM representations. When a new model becomes available, we would diff the old representation and the new representation, and then apply the resulting patchset to the actual DOM.

Looking more closely at the type `model -> VDOM`, we see that every part of the VDOM can depend on the current model. If we would like to keep the _shape_ of the page static, but allow the text and attributes to vary, we can simply move the model type into our `VDOM` type and move the function arrow under the various functors. This gives us the alternate type `SDOM` of "static DOM" elements:

```purescript
data SDOM model
  = Text (model -> String)
  | Element
      String
      (Map AttributeName (model -> AttributeValue))
      (Map EventName (model -> EventHandler))
      (Array (SDOM model))
```

This is very close to the first version of the `SDOM` data type that I defined when working on `purescript-sdom`.

This simple change to the types is enough to guarantee that only the content of text nodes, attributes and event handlers are allowed to vary with the model. Everything else must be static. We can exploit this property to render the UI only once on application startup. We can write a function in order to attach such a component to the actual DOM:

```purescript
setup 
  :: forall model
   . SDOM model
  -> DOMNode -- the DOM node to use as the root
  -> model -- the initial model
  -> Event { old :: model, new :: model } -- fires when the model changes
  -> Eff _
       { events :: Event (model -> model) -- a stream of event changes
       , unsubscribe :: Eff _ Unit -- remove event listeners
       }
```

The `Event` type here is taken from my `purescript-behaviors` library, and it represents a stream of future values.

The `setup` function could be written by pattern matching the data constructors of `SDOM`. Using it, we can build a function which "ties the knot", feeding events from our component back into the input event, causing the DOM nodes and attributes to get updated:

```purescript
attach
  :: forall model
   . Node
  -> model
  -> SDOM model
  -> Eff _ Unit
```

As we noted earlier though, this approach only works if we don't need arrays of _potentially unbounded length_. For many real applications, we're quite likely to run into this limitation, so now we'll extend this approach to deal with that case.

## Adding Support for Arrays

How can we add support for arrays of dynamic length while keeping the benefits of a static DOM? In general, we will need to be prepared to create new DOM elements if necessary (we can only preallocate a finite number, after all), but it should be possible to _reuse_ DOM elements so long as the shape of the DOM is uniform across the elements of the array.

Switching to a _generalized algebraic data type_, we can add a new data constructor for elements with a dynamic collection of children:

```purescript
data SDOM model where
  Text 
    :: (model -> String)
    -> SDOM model
  
  Element
    :: String
    -> Map AttributeName (model -> AttributeValue)
    -> Map EventName (model -> EventHandler)
    -> Array (SDOM model)
    -> SDOM model
    
  Array
    :: String -- containing element
    -> SDOM model -- element UI
    -> SDOM (Array model)
```

The type of `Array` is straightforward: given a UI for each array element, we can construct a UI for a model which is an array. The implementation of this UI can reuse child elements as needed, since they all have the same shape, or create new elements if necessary, appending them to the end of the container.

However, since PureScript does not support GADTs, I instead switched to a final encoding at this point, identifying `SDOM` values with their partial application to the `setup` function above:

```purescript
newtype SDOM model = SDOM
  (DOMNode
  -> model
  -> Event { old :: model, new :: model }
  -> Eff _
       { events :: Event (model -> model)
       , unsubscribe :: Eff _ Unit
       })
```

A value of type `SDOM model` is now a function which renders the initial `model` and then subscribes to an event in order to receive model updates. 

With this new representation, we can hide the constructor in the module's export list, and provide smart constructors, including a constructor for dynamic arrays:

```purescript
text :: forall model. (model -> String) -> SDOM model

element
  :: forall model
   . String
  -> Map AttributeName (model -> AttributeValue)
  -> Map EventName (model -> EventHandler)
  -> Array (SDOM model)
  -> SDOM model

array :: String -> SDOM model -> SDOM (Array model)
```

As it turns out, this new representation has an additional benefit: it becomes simpler to work with third-party components which are not necessarily written in PureScript, since we don't need to build a very low-level escape hatch into our initial encoding - our representation itself is now low-level.

## Context

When working with dynamic arrays, we very often need access to the index of the array element we are currently rendering. This is solved by adding _context_ to components, which I represent with an additional type argument:

```purescript
newtype SDOM context model = SDOM
  (DOMNode
  -> context
  -> model
  -> Event { old :: model, new :: model }
  -> Eff _
       { events :: Event (model -> model)
       , unsubscribe :: Eff _ Unit
       })
```

The `context` type argument is only used once, to provide an additional value which can be used during the initial setup of a component. The `context` never changes like the model does - once a component is created, its context is fixed until the component is removed.

For example, the `array` function can be modified to add the current index to the context:

```purescript
type ArrayContext context =
  { index :: Int
  , parent :: context
  }
  
array
  :: forall context model
   . String
  -> SDOM (ArrayContext context) model
  -> SDOM context (Array model)
```

The other smart constructors can be modified to make the context available:

```purescript
text 
  :: forall context model
   . (context -> model -> String)
  -> SDOM context model

element
  :: forall context model
   . String
  -> Map AttributeName (context -> model -> AttributeValue)
  -> Map EventName (context -> model -> EventHandler)
  -> Array (SDOM context model)
  -> SDOM context model
```

Notice that the shape of an element cannot depend on its context - only the values of text nodes and attributes.

## Channels

There is a second challenge when dealing with dynamic arrays. It is often useful to be able to access the array itself when defining the UI for an array _element_. For example, when creating a list of elements, it is useful to be able to attach a "remove" button to each array element, but such a button needs to modify the array itself, not just the current element.

In order to allow this, I added yet another type argument to the `SDOM` type, this time to keep track of the _event channel_:

```purescript
newtype SDOM channel context model = SDOM
  (DOMNode
  -> context
  -> model
  -> Event { old :: model, new :: model }
  -> Eff _
       { events :: Event (Either channel (model -> model))
       , unsubscribe :: Eff _ Unit
       })
```

The new `channel` type argument only appears in the type of the events that a component can raise. Now, instead of forcing the user to return a function of type `model -> model` which modifies the _current_ model type, we also allow the user to use a secondary _channel_ to send events back to a parent element.

For example, the `array` function provides a channel to its subcomponents which allows them to modify the array itself:

```purescript
data ArrayChannel model channel
  = Parent channel
  | Here (Array model -> Array model)

array
  :: forall channel context model
   . String
  -> SDOM (ArrayChannel model channel) (ArrayContext context) model
  -> SDOM channel context (Array model)
```

An `ArrayChannel` lets the user choose between applying a function `Here` to the current array, or delegating to the `Parent` channel, passing an event further up the chain.

The other smart constructors can be modified to give the user access to the event channel. For example:

```purescript
element
  :: forall channel context model
   . String
  -> Map AttributeName (context -> model -> AttributeValue)
  -> Map EventName (context -> model -> EventHandler channel) 
  -- ^ Event handlers can now use the event channel
  -> Array (SDOM channel context model)
  -> SDOM channel context model
```

This is enough to support the "remove" use case we just saw. The remove button can simply use the `Here` constructor to modify the array of elements in order to remove its own model!

## Asynchronous Events

Events channels are more generally useful, since they allow us to decouple UI components from the logic they require. For example, we can use an event channel to send an event to a parent component, without specifying how it can be interpreted. To support this sort of use case, I added the `interpretChannel` combinator:

```purescript
interpretChannel
  :: forall channel channel' context model
   . (Event channel -> Event (Either channel' (model -> model)))
  -> SDOM channel context model
  -> SDOM channel' context model
```

There is an interesting special case of `interpretChannel`, where the event channel itself uses the `Event` type to describe a stream of future values. This allows us to respond to DOM events asynchronously, or to respond with multiple model updates over time. The `withAsync` combinator facilitates this use case:

```purescript
withAsync
  :: forall channel context model
   . SDOM (Event (Either channel (model -> model))) context model
  -> SDOM channel context model
withAsync = interpretChannel keepLatest
```

This gives us lots of expressive power, since the entire language of `Event`s (and `Behavior`s) becomes available for describing component behavior.

## Focusing State

The final version of the `SDOM` type involves just one more type argument (!)

However, this time, I am not really adding a new type argument so much as splitting an existing one into two. This is one of my favorite tricks, since it extends our API with the expressiveness of _profunctor lenses_, almost for free.

Let's split the `model` type into two arguments, one for covariant uses (I'll call this `o` for "output"), and the other for contravariant uses (called `i` for "input"):

```purescript
newtype SDOM channel context i o = SDOM
  (DOMNode
  -> context
  -> i
  -> Event { old :: i, new :: i }
  -> Eff _
       { events :: Event (Either channel (i -> o))
       , unsubscribe :: Eff _ Unit
       })
```

This makes `SDOM channel context` into a _profunctor_:

```purescript
instance profunctorSDOM :: Profunctor (SDOM channel context)
```

More importantly, it is also a _strong profunctor_:

```purescript
instance strongSDOM :: Strong (SDOM channel context)
```

Now consider the definition of a profunctor lens:

```purescript
type Lens s t a b = forall p. Strong p => p a b -> p s t
```

We can see that it is now possible to apply a profunctor lens directly to a value of type `SDOM`, since we can simply _instantiate_ the type variable `p` with the type `SDOM channel context`, giving us a very useful combinator for free:

```purescript
focus 
  :: forall s t a b
   . Lens s t a b
  -> SDOM channel context a b
  -> SDOM channel context s t
focus l = l
```

This combinator is just the identity function, but it is very useful in practice. If we only want to use a subset of the current model type, we can simply apply a `Lens` to focus on that part. This lets us write our components in such a way that we only ever deal with the absolute minimum amount of state.

## Conclusion

That's all there is to the `purescript-sdom` library. I've enjoyed working on it because many of the decisions were made automatically, by observing any issues and then simply following the types, as I've tried to illustrate here.

If you'd like to help me work on the library, please let me know. There's still plenty to do!

## Addendum: Incremental Rendering

The `purescript-sdom` approach has some limitations of its own. Most notably, the `array` function is optimized for array additions and removals which happen at the end of the array, so that array elements near the start are reused. For modifications away from the end of the array, we can end up with many more DOM updates than absolutely needed. In practice, this is mostly fine, but there are corner cases where it could become a performance issue. For that reason, I'm also working on another approach based on the _incremental lambda calculus_.

The incremental lambda calculus is a model of lambda calculus in which each type is associated with a type of _changes_, and in which each function is _differentiable_, in the sense that we can produce a function on the level of changes which allows us to incrementally compute changes in results from changes in inputs.

This is a good fit for a UI library, since we can define a UI as an incremental function from some model type to a type of DOM elements. The incremental nature of functions means that we can turn model changes directly into DOM changes without having to diff any sort of virtual DOM representation (clearly, there is an overlap with `purescript-sdom` here).

We can endow arrays with a type of changes which support arbitrary insertions and removals, and then translate those to change on the DOM. This way, we don't need to worry any more about updates away from the end of an array.

I hope to have something to report about this approach soon.
