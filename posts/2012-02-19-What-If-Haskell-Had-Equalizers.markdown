---
title: What if Haskell had (co)-equalizers?
author: Phil Freeman
date: 2012/02/19
description: A thought-experiment on the possibility of extending the Haskell type system with equalizers and coequalizers.
tags: Haskell, Category Theory
---

Equalizers and coequalizers are two categorical constructions that I have never been fully comfortable with. I understand them in theory, such as how products and equalizers can be used to construct all finite limits, and I understand their interpretations in the category of sets, but I could never see how they should really be used in general. So I decided to try to interpret them via the Curry-Howard isomorphism in the hope of gaining a bit more understanding, hence the title of this post.

## Equalizers

Let\'s look at equalizers first. Let\'s take two types `X` and `Y`, and two functions `f, g :: X -> Y`.

In any category, equalizers are monos, so I expect the equalizer of `f` and `g` to look like a subset of the inhabitants of `X`.

An equalizer of f and g will consist of a type `E` and a map `e :: E -> X` such that `f e = g e`. Moreover, `(E, e)` will be universal with respect to this property.

That is, given any `(E', e')` such that `f e' = g e'`, there will be an induced map `e' -> e` such that the obvious diagram commutes.

How might we constuct an element of `E`? Well, the universal property of `E` gives one answer. We can construct an element of `E` by producing such a pair `(E', e')`:

~~~
data E f g where 
  E :: forall E'. E' -> (e' :: E' -> X) -> (f . e' = g . e') -> E
~~~

This is obviously not valid Haskell. The last argument in the constructor of `E` is a fictional addition to the Haskell type system. It is a witness to the commutativity of a particular diagram.

That is, one would only be able to construct a member of the type `E` by providing a type `E'`, a morphism `e'` which equalizes `f` and `g`, and a witness to that fact.

We would be able to destruct a value in the equalizer as follows:

~~~
e :: E -> X
e (E x e' _) = e' x
~~~

Moreover, we would know that `f x = g x` for every `x` in the image of `e`. This might be available to the compiler as an optimizing transformation.

So, as expected, equalizers correspond to subtypes or refinement types. What about coequalizers?

## Coequalizers

Given types `X` and `Y`, and functions `f, g :: X -> Y`, the coequalizer of `f` and `g` will consist of a type `C` and a function `c :: Y -> C` such that `c f = c g`.

`(C, c)` will be universal with respect to this property. If `C'`, `c' :: Y -> C'` also satisfies `f c' = g c'` then there will be an induced function `c -> c'` (and a matching commutative diagram).

This time, the universal property gives an answer to how we might destruct a value in the coequalizer. We can identify an inhabitant of the coequalizer with the function it induces:

~~~
data C f g where 
  C :: (forall C'. (c' :: Y -> C') -> (c' . f = c' . g) -> C') -> C
~~~

Now, to destruct a value in `C`, we need to pass in a type `C'`, a function `c' :: Y -> C'`, and a witness to the fact that `c'` coequalizes `f` and `g`.

We would be able to construct a value in `C` from any value in `Y`:

~~~
c :: Y -> C f g
c y = C (\c' _ = c' y)
~~~

Now as an example, take `X` and `Y` to be the type `(A, A)` of pairs of elements of type `A`. Take `f` to be the identity function, and `g` to be the function which swaps the components of the pair.

To destruct a value in the coequalizer, we need to provide a target type, a function from pairs of element of `A` to the target type, and a proof that our function is invariant under swapping the elements in the pair. For example, we might define a function to sum a pair of integers:

~~~
sum (C f) = f (uncurry (+)) ?
~~~

where the question mark represents a proof of the commutativity of addition.

We can equate this coequalizer to the type of unordered pairs of elements of `A`. It is a quotient of the type of ordered pairs `(A, A)` and the function `c` is the projection to the quotient. This allows us to capture invariants of types, as with constructions such as nested datatypes, but this way the projection function is made explicit.

In a type system with coequalizers, we would be able to define all sorts of interesting quotient types: quotienting the type of lists by appropriate functions would allow us to define things like bags and sets for example.

## Conclusion

So, it seems that adding equalizers and coequalizers to a cartesian closed category amounts to enriching the corresponding type system with refinement types and quotient types respectively.

This thought experiment used Haskell only as a means to communicate its key ideas, but was a good example of how the Curry Howard isomorphism (in this case, the appropriate side of the triangle is between type systems and cartesian closed categories) can be helpful when trying to understand some categorical concepts.
