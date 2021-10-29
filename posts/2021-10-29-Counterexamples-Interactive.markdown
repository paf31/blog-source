---
title: Counterexamples of Type Classes, Interactive!
author: Phil Freeman
date: 2021/10/29
description:
tags: Haskell
---


<link rel="stylesheet" href="../assets/counterexamples.css" type="text/css"></link>

<script src="https://d3js.org/d3.v5.min.js"></script>

<script src="https://unpkg.com/@hpcc-js/wasm@0.3.11/dist/index.min.js"></script>

<script src="https://unpkg.com/d3-graphviz@3.0.5/build/d3-graphviz.js"></script>

This is an updated version of my [previous blog post](2015-12-06-Counterexamples.html), in which I have reorganized the data in the form of a graph to be more easily discoverable. You can click on any node or edge in the graph below to reveal more information about that node (type class) or edge (superclass relationship, including relevant examples and counterexamples).

---

In Haskell and PureScript, we are familiar with certain standard hierarchies of type classes. For example, here are some of the type classes provided by the PureScript core libraries:

- `Semigroup`, `Monoid`
- `Semiring`, `Ring`, `CommutativeRing`, `EuclideanRing`, `Field`
- `Functor`, `Apply`, `Applicative`, `Bind`, `Monad`
- `Extend`, `Comonad`
- `Alt`, `Plus`, `Alternative`
- `Semigroupoid`, `Category`
- `Profunctor`, `Strong`, `Arrow`

For each of these hierarchies, we have examples of each class, and in many cases, free constructions for superclass relationships.

However, it is also instructive to look at _counterexamples_, of types which inhabit a superclass, but not a subclass, to understand why these refinements are useful.

<div id="graph" style="text-align: center;"></div>

<div class="section" id="section_Monoid">

## The Monoid Hierarchy

The `Monoid` type class from Haskell is refined in PureScript, by splitting out the `append` operation into the `Semigroup` class:

```text
class Semigroup m where
  append :: m -> m -> m

class (Semigroup m) <= Monoid m where
  mempty :: m
```

</div>
<div class="section" id="section_SemigroupMonoid">

## A Semigroup which is not a Monoid

Non-empty lists form a `Semigroup`, where `append` is given by concatenating two lists:

```text
data NonEmpty a = NonEmpty a (List a)
```

In fact, `NonEmpty a` is the _free semigroup_ for the type `a`.

</div>
<div class="section" id="section_Semigroup">

## Notes on Semigroup

In general, structures which lack empty elements might have instances of `Semigroup` but not of `Monoid`.

The free construction of a `Monoid` from a `Semigroup` is to simply add an empty element:

```text
data FreeMonoid s = Empty | NonEmpty s

instance freeMonoidFromSemigroup :: (Semigroup s) => Monoid (FreeMonoid s)
```

The two free constructions `NonEmpty` and `FreeMonoid` compose to give something isomorphic to `List a`, the free `Monoid` for the type `a`.

An example of an operation which typically uses a `Monoid` instance, but which only _requires_ a `Semigroup` is Haskell's `foldl1` from `Data.List`. Since we are always folding at least one element, we only need an `append` operation, not an empty element.

```text
foldl1 :: forall s. (Semigroup s) => NonEmpty s -> s
```

The applicative validation functor provides an example of a data structure which can be generalized to work with any Semigroup:

```text
data Validation s a = Validation (Either s a)

instance (Semigroup s) => Applicative (Validation s)
```

Here, we can use the `append` operation to collect multiple errors, but we do not require a full `Monoid`. For example, `Validation (NonEmpty String)` might be used to collect a non-empty list of validation errors represented as strings.

`Validation` is _not_ a `Monad`, unlike `Either`, since there is no implementation of `>>=` which is compatible with the `Applicative` implementation.

</div>
<div class="section" id="section_Field">

## The Field Hierarchy

The `Field` type class from Haskell is refined into a number of type classes in PureScript:

```text
class Semiring a where
  add  :: a -> a -> a
  zero :: a
  mul  :: a -> a -> a
  one  :: a

class Semiring a <= Ring a where
  sub :: a -> a -> a

class Ring a <= CommutativeRing a where

class CommutativeRing a <= EuclideanRing a where
  div :: a -> a -> a
  mod :: a -> a -> a
  degree :: a -> Int

class Ring a <= DivisionRing a where
  recip :: a -> a

class EuclideanRing a <= Field a
```

The `Semiring` class specifies the addition and multiplication operations, while subtraction is split into the `Ring` class.

Commutative rings are specified by the `CommutativeRing` class.

Division and the modulus operator are broken into the `EuclideanRing` class.

`DivisionRing` specifies an alternative to division in terms of a reciprocal function, and requires that `recip` provides multiplicative inverses. A `DivisionRing` need not be commutative.

Finally, `Field` requires that multiplication is commutative.

</div>
<div class="section" id="section_SemiringRing">

## A Semiring which is not a Ring

Natural numbers form a `Semiring`, but do not form a `Ring`, since they lack additive inverses:

```text
data Nat = Zero | Succ Nat
```

</div>
<div class="section" id="section_RingDivisionRing">

## A Ring which is not a DivisionRing

The integers provide an example of a `Ring` which is not a `DivisionRing`, since non-zero integers do not have multiplicative inverses.

The integers are also an example of a `EuclideanRing` which is not a `Field`.

</div>
<div class="section" id="section_CommutativeRingEuclideanRing">

## A CommutativeRing which is not a EuclideanRing

Number theory provides examples of commutative rings which fail to be Euclidean. For example, rings which are not principal ideal domains cannot be Euclidean.

Wikipedia provides [some additional counterexamples](https://en.wikipedia.org/wiki/Euclidean_domain#Examples).

</div>
<div class="section" id="section_CommutativeRingDivisionRing">

## A DivisionRing which is not a CommutativeRing

The quaternions are an example of a structure with multiplicative inverses but non-commutative multiplication. They form a `DivisionRing`, but not a `CommutativeRing`.

</div>
<div class="section" id="section_Semiring">

## Notes on Semiring

There is a free `Semiring` which can be constructed from any base type, formed by using distributivity to represent terms in "`+`-normal form":

```text
data FreeSemiring a = FreeSemiring (List (List a))

instance freeSemiring :: Semiring (FreeSemiring a)
```

This type is implemented in the `purescript-semirings` library.

The free semiring may be interpreted in any semiring:

```text
liftFree :: forall s a. (Semiring s) => (a -> s) -> Free a -> s
```

giving a form of abstract interpretation of the semiring operations.

Here are two examples of data structures which can be usefully generalized to work with an arbitrary `Semiring`:

- The familiar probability monad `newtype Prob a = Prob (List (Tuple Number a))` can be generalized to use a `Semiring` of probabilities: `data GProb s a = GProb (List (Tuple s a))`. Choosing different `Semiring`s recovers other less-familiar monads such as a monad for describing quantum wavefunctions (take `s` to be the Semiring of complex numbers) and a monad for managing priority queues (take `s` to be the (`Max`, `+`) Semiring).
- The applicative validation functor has an `Alternative` instance for collecting errors in parallel whenever the errors form a Semiring.

</div>
<div class="section" id="section_Ring">

## Notes on Ring

We can construct a `Ring` from any `Semiring` in the same way as we construct the integers from the naturals, by representing _differences_:

```text
data Diff a = Diff a a

instance Semiring a => Ring (Diff a)
```

We identify any two values of type `Diff a` which represent the same difference.

</div>
<div class="section" id="section_DivisionRing">

## Notes on DivisionRing

We can formally add divisors to any `Ring` in much the same way:

```text
data Ratio a = Ratio a a

instance Ring a => DivisionRing (Ratio a)
```

We disallow zero divisors, and identify any two values of type `Ratio a` which represent the same ratio.

</div>
<div class="section" id="section_Monad">

## The Monad Hierarchy

Haskell's `Monad` hierarchy is further refined in PureScript, by the addition of the `Apply` class which contains the `<*>` operator (but not `pure`) and the `Bind` class which specifies the `>>=` operator:

```text
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

class Functor f <= Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b

class Apply f <= Applicative f where
  pure :: forall a. a -> f a

class Apply m <= Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b

class (Applicative m, Bind m) <= Monad m
```

</div>
<div class="section" id="section_ConFunctor">

## A type constructor which is not a Functor

Functor forces the type variable of its argument to appear covariantly, so a simple example of a type constructor which is not a Functor is given by:

```text
data Op a b = Op (b -> a)
```

`Op` can be made into a contravariant functor, described by the `Contravariant` type class, which we will not cover here.

</div>
<div class="section" id="section_FunctorApply">

## A Functor which is not an Apply

There is a functor for any type constructor, given by the Yoneda lemma:

```text
newtype Yoneda f a = Yoneda (forall r. (a -> r) -> f r)
```

`Yoneda f` is always a `Functor`, but cannot be made into an instance of `Apply` in general, since to implement `<*>`, we would require a function of type:

```text
forall a b r. (forall r. ((a -> b) -> r) -> f r) ->
              (forall r. (a        -> r) -> f r) ->
                         (b        -> r) -> f r
```

</div>
<div class="section" id="section_ApplyApplicative">

## An Apply which is not an Applicative

The constant functor

```text
data Const k a = Const k
```

can be made into an `Apply` (but not an `Applicative`) whenever `k` is an instance of `Semigroup` (but not `Monoid`).

</div>
<div class="section" id="+ section_ApplicativeMonad">

## An Applicative which is not a Monad

We have already seen that the validation functor is an example of an `Applicative` which is not a `Monad`, and therefore also an example of an `Apply` which is not a `Bind`.

Another example is given by the `ZipList` functor, where `apply` is implemented using `zipWith`:

```haskell
newtype ZipList a = ZipList (List a)
```

</div>
<div class="section" id="section_BindMonad">

## A Bind which is not a Monad

The `Map`k data type has an implementation of `Bind`, since we can implement the `map`, `apply` and `bind` functions, but fails to be a `Monad`, since we cannot implement

```text
pure :: forall k a. a -> Map k a
```

such that the monad laws will hold.

</div>
<div class="section" id="section_Apply">

## Notes on Apply

`Apply` is enough to implement a variant of `zip`:

```text
zipA :: forall f a b. (Apply f) => f a -> f b -> f (Tuple a b)
```

where we can recover the original `zip` function by using the `ZipList` applicative.

</div>
<div class="section" id="section_Bind">

## Notes on Bind

`Bind` is enough to implement Kleisli composition:

```text
(>=>) :: forall f a b c. (Bind f) => (a -> f b) -> (b -> f c) -> a -> f c
```

</div>
<div class="section" id="section_Comonad">

## The Comonad Hierarchy

PureScript refines the `Comonad` type class to extract the `extend` function into its own `Extend` class:

```text
class Functor w <= Extend w where
  extend :: forall b a. (w a -> b) -> w a -> w b

class Extend w <= Comonad w where
  extract :: forall a. w a -> a
```

</div>
<div class="section" id="section_ExtendComonad">

## An Extend which is not a Comonad

The function type gives an example of a `Comonad` whenever the domain is monoidal. If we relax the constraint to `Semigroup`, however, we only obtain an `Extend`:

```text
instance extendFn :: Semigroup w => Extend ((->) w)
```

For example, the `NonEmpty Unit` semigroup can be thought of as non-zero natural numbers, and gives us an `Extend` instance which allows us to reannotate a semi-infinite tape by considering values (strictly) to the right of the current position.

</div>
<div class="section" id="section_Extend">

## Notes on Extend

`Extend` is enough to implement co-Kleisli composition:

```text
(=>=) :: forall b a w c. (Extend w) => (w a -> b) -> (w b -> c) -> w a -> c
```

</div>
<div class="section" id="+ section_Alternative">

## The Alternative Hierarchy

In PureScript, the `Alternative` class is refined so that the alternation operator `<|>` is broken out into the `Alt` class, the `empty` structure is defined by the `Plus` class, and `Alternative` is redefined as a combination of `Applicative` and `Plus`:

```text
class Functor f <= Alt f where
  alt :: forall a. f a -> f a -> f a

class Alt f <= Plus f where
  empty :: forall a. f a

class (Applicative f, Plus f) <= Alternative f
```

</div>
<div class="section" id="section_AltPlus">

## An Alt which is not a Plus

`NonEmpty` is an example of an `Alt` in which `alt` is given by concatenation, but there is no `Plus` instance due to the lack of an empty element.

</div>
<div class="section" id="section_PlusAlternative">

## A Plus which is not an Alternative

`Map k` is an example of a `Plus` which is not `Alternative`, due to the lack of an `Applicative` instance

</div>
<div class="section" id="section_Alt">

## Notes on Alt

`Alt` is enough to define `oneOf1`, a variant on `oneOf` which requires at least one alternative:

```text
oneOf1 :: forall f a. (Alt f) => NonEmpty (f a) -> f a
```

Any `Alt` can be used to construct a `Semigroup` (but not necessarily a `Monoid`) using `<|>`.

</div>
<div class="section" id="section_Plus">

## Notes on Plus

`Plus` is enough to define `oneOf`:

```text
oneOf :: forall f a. (Plus f) => List (f a) -> f a
```

</div>
<div class="section" id="section_Category">

## The Category Hierarchy

In PureScript, the `Category` type class is refined by splitting the `compose` operation into the `Semigroupoid` class:

```text
class Semigroupoid a where
  compose :: forall b c d. a c d -> a b c -> a b d

class Semigroupoid a <= Category a where
  id :: forall t. a t t
```

</div>
<div class="section" id="section_SemigroupoidCategory">

## A Semigroupoid which is not a Category

`Tuple` is an example of a `Semigroupoid` which is not a `Category`. We can think of `Tuple` as a strange function space in which we identify only a single point in the domain and a single point in the codomain.

In the same vein, another example is given by

```text
newtype Relation a b = Relation (List (Tuple a b))
```

in which we choose a selection of pairs of points from the domain and codomain. Again, there is no `Category` instance, since we cannot construct

```
id :: forall a. Relation a a
```

in general, unless both the domain and codomain are enumerable.

Kleisli arrows form a `Semigroupoid` whenever `f` is a `Bind`, but not a `Category` in general, unless `f` is also a `Monad`:

```text
newtype Kleisli f a b = Kleisli (a -> f b)
newtype Cokleisli f a b = Cokleisli (f a -> b)
```

A similar construction creates a `Semigroupoid` on `Cokleisli f` whenever `f` has `Extend` (but not necessarily `Comonad`).

Static arrows `f (a -> b)` form a `Semigroupoid` whenever `f` is an `Apply`, but not a `Category` in general, unless `f` is also `Applicative`:

```text
newtype Static f a b = Static (f (a -> b))
```

</div>
<div class="section" id="section_Semigroupoid">

## Notes on Semigroupoid

Any `Semigroupoid` supports the composition of a `NonEmpty` list of morphisms:

```text
compose1 :: forall s a. (Semigroupoid s) => NonEmpty (s a a) -> s a a
```

</div>
<div class="section" id="section_Arrow">

## The Arrow Hierarchy

In PureScript, the `Arrow` type class is defined in terms of a combination of `Category` and strong profunctors:

```text
class Profunctor p where
  dimap :: forall a b c d. (a -> b) -> (c -> d) -> p b c -> p a d

class Profunctor p <= Strong p where
  first :: forall a b c. p a b -> p (Tuple a c) (Tuple b c)

class (Category a, Strong a) <= Arrow a
```

</div>
<div class="section" id="section_ProfunctorStrong">

## A Profunctor which is not Strong

Here is a `Profunctor` which ignores its first type argument, so parametericity makes it impossible to define `first`:

```text
newtype Ignore a b = Ignore b
```

</div>
<div class="section" id="section_StrongArrow">

## A Strong Profunctor which is not an Arrow

`Kleisli f` is `Strong` whenever `f` is a `Functor`, but cannot be an `Arrow` unless `f` is a `Monad`, due to the lack of a `Category` instance.

</div>
<div class="section" id="section_CategoryArrow">

## A Category which is not an Arrow

Isomorphisms form a `Category`:

```text
data Iso a b = Iso { to :: a -> b, from :: b -> a }
```

But `Iso` cannot be a `Profunctor` since `a` appears both covariantly and contravariantly.

</div>
<div class="section" id="section_Profunctor">

## Notes on Profunctor

A `Profunctor` supports a change of types using any _isomorphism_:

```text
data Iso s a = Iso { to :: s -> a, from :: a -> s }

liftIso :: forall p a s. (Profunctor p) => Iso s a -> p a a -> p s s
```

</div>
<div class="section" id="section_Strong">

## Notes on Strong

A `Strong` profunctor supports a focussing operation using any _lens_:

```text
data Lens s a = Lens { get :: s -> a, set :: s -> a -> s }

liftLens :: forall p a s. (Strong p) => Lens s a -> p a a -> p s s
```

</div>

<script src="../assets/jquery.min.js" type="text/javascript"></script>

<script src="../assets/counterexamples.js" type="text/javascript"></script>