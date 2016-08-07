---
title: Comonads as Spaces
author: Phil Freeman
date: 2016/08/07
description:
tags: Haskell
---

I feel like I've had a good intuition for monads for some time, but I've struggled to really get
a handle on comonads until recently. I'd like to present an application of comonads which I think
provides a helpful intuition, and then interpret several comonads in that setting.

### Introduction

In PureScript, we have several user interface libraries. They are all slightly different, but have
some things in common. Many of them are based on underlying libraries like React and virtual-dom,
and the approach always boils down to maintaining some component state, and updating that state in response to user actions. The libraries tend to differ in how that state is presented to the
developer - some libraries present a very low-level API (e.g. `purescript-react`), while others
provide a more restricted API (e.g. Thermite, Halogen). I would like to show one way in which these different approaches can be unified.

### Functors and Pairings

Before we talk about comonads, I'd like to give an overview of _functor pairings_.

Consider the `uncurry` function:

```haskell
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b
```

We can think of this function as a relationship between the `(->) a` and `(,) a` functors, where
one functor wraps a function type, the other functor wraps a value in the domain of that function type, and the two structures annihilate each other to produce a value in the codomain.

The two functors are still related when we switch their roles:

```haskell
cocurry :: (a, b -> c) -> (a -> b) -> c
cocurry (a, bc) ab = bc (ab a)
```

We can generalize this to a _pairing_ between two functors as follows:

```haskell
type Pairing f g = forall a b. f (a -> b) -> g b -> c

uncurry :: Pairing ((->) a) ((,) a)
cocurry :: Pairing ((,) a) ((->) a)
```

Edward Kmett [has written about pairings before](http://comonad.com/reader/2008/the-cofree-comonad-and-the-expression-problem/), and his `adjunctions` library contains the [relevant code](http://hackage.haskell.org/package/adjunctions-0.6.0/docs/Data-Functor-Zap.html). There is also a [PureScript implementation](https://pursuit.purescript.org/packages/purescript-pairing/1.4.0/docs/Data.Functor.Pairing) available.

### Examples of Pairings

There are plenty of examples of pairings between functors.

The `Identity` functor pairs with itself.

`Coproduct f1 g1` pairs with `Product f2 g2` whenever `f1` pairs with `f2`, and `g1` pairs with `g2`:

```haskell
data Product f g a = Product (f a) (g a)

data Coproduct f g a = Inl (f a) | Inr (g a)

pair :: Pairing f1 f2
     -> Pairing g1 g2
     -> Pairing (Product f1 g1) (Coproduct f2 g2)
pair f _ (Product f1 _ ) (Inl f2) = f f1 f2
pair _ g (Product _  g1) (Inr g2) = g g1 g2

pair' :: Pairing f1 f2
      -> Pairing g1 g2
      -> Pairing (Coproduct f1 g1) (Product f2 g2)
pair' f _ (Inl f1) (Product f2 _ ) = f f1 f2
pair' _ g (Inr g2) (Product _  g2) = g g1 g2
```

Compositions of functors pair whenever the corresponding components pair (from now on, I'll omit the implementations):

```haskell
data Compose f g a = Compose (f (g a))

pair :: Pairing f1 f2
     -> Pairing g1 g2
     -> Pairing (Compose f1 g1) (Compose f2 g2)
```

### Pairings of Monads and Comonads

Plenty of monads and comonads provide interesting examples of pairings:

- The `State s` monad pairs with the `Store s` comonad. In fact, `StateT s m` pairs with `StoreT s w` whenever `m` pairs with `m`.
- `WriterT a m` pairs with `TracedT a w` whenever `m` pairs with `w`.
- `ReaderT r m` pairs with `EnvT r w` whenever `m` pairs with `w`.
- `Free f` pairs with `Cofree g` whenever `f` pairs with `g`.

### Monads from Comonads

Edward Kmett has written a [series of blog posts](http://comonad.com/reader/2011/monads-from-comonads/) about generating a monad (and in fact, a [monad transformer](http://comonad.com/reader/2011/monad-transformers-from-comonads/)) from every comonad.

The first blog post describes the following type:

```haskell
newtype Co w a = Co { runCo :: forall r. w (a -> r) -> r }
```

and shows that `Co w` is a `Monad` whenever `w` is a `Comonad`.

Another way of looking at this type is that `Co` is way of constructing a functor which pairs with any functor `f`, since `Co f` pairs with `f`:

```haskell
pairCo :: Pairing f (Co f)
pairCo f cof = cof f
```

(the opposite pairing is left as an exercise)

### Comonads as Spaces

I would like to think of comonads as describing _spaces_ of possible states of an application. When we come to use comonads to describe user interfaces, I will use a comonadic value to store all possible (lazily-evaluated) future states of my components.

Actually, the right intuition is that of a _pointed space_, since we will always have a notion of the "current position". We can get the state at the current position using `extract`.

Let's see some examples:

- `Store s` explicitly stores a current position of type `s`, and provides a function of type `s -> a` to extract the value at any position.
- `Traced w` uses a monoidal type `w` to describe positions. The default position is `mempty`, and a function of type `w -> a` is provided to extract the value at any position.
- `Env e` only contains a single position, which is the default position, but also packages up a read-only state of type `e`.
- `Zipper` is a comonad, and it provides a value for every position in some list.
- `Cofree f` defines a tree where the branching is determined by the functor `f`. An example is the type of `Moore` machines, isomorphic to `Cofree ((->) a)`. We can think of `Moore a` as defining a space with a value for every finite list of past inputs.

### Exploring Comonads with Pairings

A pairing between a comonad `w` and a monad `m` gives a way to explore the data in a comonadic value. We can think of this as moving around in the space described by the comonad.

We would like the actions of `m` to describe movements which we can use to modify a comonadic value of type `m a`.

To implement this, we use the `extend` function from the `Comonad` class to extend the pairing function to every comonadic subcontext:

```haskell
moving :: Comonad w => Pairing m w -> m (a -> b) -> w a -> w b
moving pair m w = extend (pair m)

move :: (Comonad w, Functor m) => Pairing m w -> m () -> w a -> w a
move pair m = moving pair (m $> id)
```

Let's see some examples.

The `Store s` comonad pairs with `State s`, so we can use `get` and `put` to read and write the current state in the store, effectively reading and updating the current position:

```haskell
moveStore :: State s () -> Store s a -> Store s a
```

The `Traced w` comonad pairs with `Writer w`, so we can use `tell` to update the monoidal position, by `mappend`ing an update to the current position.

`Env e` pairs with `Reader e`, so we can't update the position, but we can read it using `ask`.

We can use `Free g` to move around in `Cofree f`, whenever `g` pairs with `f`. For example, we can move in `Moore a` by using `Free ((,) a)`, since `((,) a)` pairs with `((->) a)`. The actions of `Free ((,) a)` consist of sequences of emitted values of type `a`, which will be fed as inputs to the Moore machine.

It's not obvious what monad to use to explore `Zipper`, but fortunately we don't need to decide. The `Co Zipper` monad pairs with `Zipper`, so we just need to find some actions in that monad! For example, we can write actions to move left and right:

```haskell
data Zipper a = Zipper [a] a [a]

instance Comonad Zipper

left :: Co Zipper ()
left = Co $ \(Zipper (f : _) _ _) -> f ()

right :: Co Zipper ()
right = Co $ \(Zipper _ _ (f : _)) -> f ()
```

To see why this is correct, consider `extend`ing these functions over an entire zipper. The effect will be that every element moves one unit to the left or right.

### Comonad Transformers

We can compose spaces by using comonad transformers instead of the basic comonads listed above.

The corresponding monad transformers will combine to provide a monad which allows us to move around in any component of our comonad transformer stack.

Alternatively, `Co` comes equipped with instances for `MonadReader`, `MonadWriter` and `MonadState`, whenever the underlying comonad provides instances for `ComonadEnv`, `ComonadTraced` and `ComonadStore` respectively. So, the lazy option is to simply apply `Co` to the entire comonad transformer stack, and use the `mtl` instances to lift the actions on the component comonads in the appropriate way!

### User Interfaces from Comonads

Now, let's see how these ideas can be applied to help us understand the problem of user interface design.

I said that I would like to think of a comonad as modeling all possible future states of our application. In each state, we will need a representation of the user interface to render. We can represent this abstractly using a type constructor `UI`:

```haskell
data UI action
```

The type argument `action` here denotes the type of user actions that we would like to support.

Well, user actions should result in movement to a new application state, so we should choose our actions from some monad which pairs with our comonad.

So, our components will be described by a comonad and a monad, and a pairing between them:

```haskell
type Component w m = w (UI (m ()))
```

That is, a space of possible states, each one of which describes a user interface, which embeds actions which can be used to explore the space of states.

We can make this go by pairing extending each monadic action over the entire current comonadic state. The main function will look something like this:

```haskell
explore :: forall w m
         . Comonad w
        => Pairing m w
        -> w (UI (m ()))
        -> IO ()
```

### UI Paradigms, Unified

I have [implemented this in PureScript](https://gist.github.com/paf31/e123ec82b33eea4458e50206326e883e) using React to wire up user actions and states.

However, I'm not interested in the implementation right now, so much as applying the general idea to different comonads.

If we apply this construction to the `Store s` comonad, then we get a model very close to pure React components: we have a type `s` of states, which we can read and write in user actions using `State s`.

The `Traced w` comonad gives a nice model for [incremental games](https://en.wikipedia.org/wiki/Incremental_game), in which the state increases in some monoid (like a click count). We can append to the state using `tell`.

The `Moore a` comonad defines a type of _actions_, and components can emit actions in response to user events. Actions act by updating the current state, but the developer only has access to the actions defined by the type `a`. This is quite similar to the approach taken in libraries like Redux, or the Elm architecture.

The `Zipper` comonad provides a model for breadcrumb components, pagination controls, or undo/redo.

The `Cofree` comonad provides a general model for components which define some typed query interface using a functor. User actions can be combined using the corresponding free monad. However, compared to the action-driven model defined by `Moore a`, the `Cofree` model allows queries to have _typed_ responses. This means that components can provide limited access to their internal state. This is very close to the approach taken in the Halogen library in PureScript.

Comonad transformers provide a way to combine components which use different models. The corresponding monad transformers combine in such a way that we are able to move around in each subcomponent independently in response to user actions.

### Comonad Morphisms

Comonad morphisms demonstrate when one model subsumes another. For example:

- `Traced w` can be interpreted in `Store w`, much like we can model `Writer w` using the `State w` monad. This means that incremental games are a special case of pure React components.
- `Moore a` can be interpreted in `Traced [a]` (exercise for the reader). This means that action-driven user interfaces are a special case of the incremental model, and in turn pure React components.
- `Cofree f` can be interpreted in `w` whenever there is a natural transformation from `f` to `w`. This means that if we can interpret each action defined by `f` in the comonad `w`, then the model defined by `w` subsumes the model defined by `Cofree f`.

So, many models are equivalent in the sense that one might be able to interpret one in terms of another. The benefit of having many models is that we can restrict the API by choosing an appropriate comonad.

### Conclusion

The approach can probably be extended to support real-world applications. For example, there is currently no support for external input or side effects in response to user actions. I'm confident those features could be added.

However, my hope is only that this approach can be used to help understanding both user interfaces and comonads. Hopefully, I've provided some intuition for comonads as spaces of states, and shown that many approaches to user interfaces can be unified under this general approach.
