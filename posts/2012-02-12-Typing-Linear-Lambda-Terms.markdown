---
title: Typing Linear Lambda Terms
author: Phil Freeman
date: 2012/02/12
description: An encoding of linear lambda terms in Haskell which prevents the expression of invalid terms.
tags: Haskell, PolyKinds
---

In this post, I\'d like to look at the problem of encoding linear lambda terms in Haskell. Specifically, I\'d like to look at how to restrict the class of expressible lambda terms to only allow variables to be applied exactly once.

For example, the combinator `K x y = x` should be disallowed because the variable `y` is never used, and the combinator `S x y z = x z (y z)` should be disallowed because the variable `z` is used more than once.

The combinators `S` and `K` form a basis for the set of unrestricted lambda terms, so one should ask, is there anything interesting left after they are removed? Indeed, one can find a useful basis for the class of linear lambda terms, which we will express in Haskell below.

~~~{.haskell}
{-# LANGUAGE RankNTypes, DataKinds, GADTs, TypeOperators #-}

module Lin where
~~~

This definition of linear lambda terms depends crucially on the definition of a splitting of a set of variables.

The thing which prevents `S` from being an inhabitant of our desired type is that the variable occurs on each side of the application `(x z) (y z)`.

We need to ensure that each variable only occurs on one side of each application, and this motivates the following definition of a splitting of a set of variables `s` into two disjoint sets `v1`, `v2` of variables whose union is `s`. The only splitting of the empty set is `E`, the empty splitting, and one can obtain a splitting of `(v:vs)` from a splitting of `vs` by adding `v` to either the left set `v1` or to the right set `v2`:

~~~{.haskell}
data Splitting v1 v2 s where
  E :: Splitting '[] '[] '[]
  L :: Splitting xs ys s -> Splitting (x ': xs) ys (x ': s)
  R :: Splitting xs ys s -> Splitting xs (y ': ys) (y ': s)
~~~

With that, here is the definition of the set of linear lambda terms. The definition looks similar to a definition of unrestricted lambda terms, modulo extra type constraints.

~~~{.haskell}
data Lin vars where
  Var :: v -> Lin (v ': '[])
  App :: Splitting vs ws s -> Lin vs -> Lin ws -> Lin s
  Abs :: (forall v. v -> Lin (v ': vars)) -> Lin vars
~~~

The type parameter vars has kind `[*]` and represents the bound variables appearing in the term.

An abstraction introduces a new bound variable, and quantification over the type of the new bound variable prevents the user from constructing invalid terms by instantiating a `Var` for a different variable type `v`.

An application splits the set of bound variables into two pieces. Constructing an application term requires the user to specify the splitting explictly as an argument.

A variable can only be constructed in the context of a single bound variable - this disallows terms such as `K` where we have a bound variable left over.

Finally, a closed linear lambda term is a linear lambda term with no free variables:

~~~{.haskell}
type Closed = Lin '[]
~~~

Now we can express some linear lambda terms. The identity is the simplest example:

~~~{.haskell}
-- I x = x
i = Abs (\x -Var x)
~~~

The `B` and `C` combinators are a little trickier, since one has to define the splitting of the set of bound variables for every application. For example, the first application in `B` says "`x` goes on the left of the application, `y` and `z` go on the right".

~~~{.haskell}
-- B x y z = x (y z)
b = Abs (\x -> Abs (\y -> Abs (\z -> App (R $ R $ L $ E) (Var x) (App (R $ L $ E) (Var y) (Var z)))))

-- C x y z = x z y
c = Abs (\x -> Abs (\y -> Abs (\z -> App (L $ R $ L $ E) (App (R $ L $ E) (Var x) (Var z)) (Var y))))
~~~

As an example of something which cannot be typed as a linear lambda term, an attempt to define the `K` combinator fails with this error message:

~~~
-- K x y = x
-- k = Abs (\x -> Abs (\y -> Var x))
--
--  Couldn't match expected type `(:) * var vars0'
--              with actual type `[] *'
--  Expected type: Lin ((:) * var1 ((:) * var vars0))
--    Actual type: Lin ((:) * var1 ([] *))
--  In the return type of a call of `Var'
--  In the expression: Var x
~~~

This error indicates that we have one too many type variables left over at the node of the tree where `Var x` appears.
