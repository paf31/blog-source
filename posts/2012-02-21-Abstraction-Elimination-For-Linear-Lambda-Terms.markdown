---
title: Abstraction Elimination For Linear Lambda Terms
author: Phil Freeman
date: 2012/02/21
description: An abstraction elimination for linear lambda terms
tags: Haskell, PolyKinds
---

Last time, I wrote a little bit about encoding linear lambda terms in Haskell using promoted datatypes to represent the set of variables appearing in a term. This time I\'d like to look at an algorithm for abstraction elimination for linear lambda terms.

~~~{.haskell}
{-# LANGUAGE RankNTypes, DataKinds, GADTs, TypeOperators, FlexibleInstances #-

module Elim where
~~~

Abstraction elimination is the process by which lambda expressions are converted into pointfree expressions. In the simply typed lambda calculus, abstraction elimination uses the basis `{S, K, I}`. However, in the linear case the combinators `S` and `K` cannot be expressed as we saw last time. This time, we will see that the combinators `B`, `C` and `I` form a basis for the linear terms.

The typical abstraction elimination algorithm basically replaces applications with the combinator `S`, and variables with the combinator `I`. The `K` combinator is used in the case when a variable does not appear in a subexpression, but in the linear case this will never occur. The application case is split into two smaller cases: while eliminating a variable `x` at an application, `x` can appear on the left of the application or on the right, but not in both.

Looking at the definitions of the combinators `B` and `C`, we can see how they correspond to these two cases. In `B`, the third argument `z` only appears on the right, and in `C` it only appears on the left:

~~~
-- ghci> let b' = \x y z -> x (y z)
-- ghci> let c' = \x y z -> (x z) y
-- 
-- ghci> :t b'
-- (t1 -> t) -> (t2 -> t1) -> t2 -> t
-- ghci> :t c'
-- (t1 -> t2 -> t) -> t2 -> t1 -> t
~~~

Let's recall the code from last time. I defined a type of context splittings, used to divide up the variables in an application into two subsets:

~~~{.haskell}
data Splitting v1 v2 s where
  E :: Splitting '[] '[] '[]
  L :: Splitting xs ys s -Splitting (x ': xs) ys (x ': s)
  R :: Splitting xs ys s -Splitting xs (y ': ys) (y ': s)
~~~

I then defined the following type of linear terms, and a few inhabitants:

~~~{.haskell}
data Lin vars where
  Var :: v -Lin (v ': '[])
  App :: Splitting vs ws s -Lin vs -Lin ws -Lin s
  Abs :: (forall v. v -Lin (v ': vars)) -Lin vars

i = Abs (\x -> Var x)

b = Abs (\x -> Abs (\y -> Abs (\z -> App (R $ R $ L $ E) (Var x) (App (R $ L $ E) (Var y) (Var z)))))

c = Abs (\x -> Abs (\y -> Abs (\z -> App (L $ R $ L $ E) (App (R $ L $ E) (Var x) (Var z)) (Var y))))
~~~

Now, we will show that these three terms form a basis for all linear terms. That is, these three terms, along with applications, are enough to express all linear lambda terms.

We seek a function from the type `Lin` to trees which only mention applications and the three terms `B`, `C` and `I`. Let's define an extended version of the type `Lin`, which adds three constructors for the three combinators:

~~~{.haskell}
data Lin' vars where
  B :: Lin' '[]
  C :: Lin' '[]
  I :: Lin' '[]
  Var' :: v -Lin' (v ': '[])
  App' :: Splitting vs ws s -Lin' vs -Lin' ws -Lin' s
  Abs' :: (forall v. v -Lin' (v ': vars)) -Lin' vars

instance Show (Lin' '[]) where
  show B = "B"
  show C = "C"
  show I = "I"
  show (App' E t1 t2) = "(" ++ (show t1) ++ " " ++ (show t2) ++ ")"
~~~

The idea is to progressively replace the `Abs'` and `Var'` constructors with the remaining constructors, until there are no more abstractions appearing in the expression, and therefore no more variables.

We will walk the expression tree looking for abstractions. When one is found, we have a polymorphic function with type

~~~
-- forall v. v -> Lin' (v ': vars)
~~~

We can pass in a name for the variable `v` and get back another expression. Parametricity forces the name of all instances of the variable `v` in this expression to be equal to the value we passed in to the polymorphic function, so we can look for that value, and replace it with the `I` combinator recursively.

We need a supply of unique variable names:

~~~{.haskell}
newtype VarSupply a = VarSupply { runSupply :: Int -> (a, Int) }

instance Monad VarSupply where
  return a = VarSupply (\i -> (a, i))
  f >>= g = VarSupply (\i -> let (a, j) = runSupply f i in runSupply (g a) j)
~~~

This action generates a new variable name:

~~~{.haskell}
nextVar :: VarSupply Int
nextVar = VarSupply (\i -> (i, i + 1))
~~~

The final piece before we can implement the elimination function is a helper function used to create context splittings. This function creates trivial splittings with all variables on the right. These will be used to construct applications of the combinators `B` and `C`, which mention no variables at all (and so all variables in the application must occur in the right hand subexpression):

~~~{.haskell}
trivial :: Lin' vars -> Splitting '[] vars vars
trivial (Var' _) = R $ E
trivial (App' s t1 t2) = trivial' s where
  trivial' :: Splitting v1 v2 s -> Splitting '[] s s
  trivial' E = E
  trivial' (L s) = R $ trivial' s
  trivial' (R s) = R $ trivial' s
trivial (Abs' f) = case trivial (f ()) of (R s) -> s
trivial B = E
trivial C = E
trivial I = E
~~~

Now we can define the elimination function:

~~~{.haskell}
elim :: Lin vars -> Lin' vars
elim t = fst $ runSupply (elim' $ inj t) 0 where
~~~

The helper function `inj` injects values in the type `Lin` into the type `Lin'`:

~~~{.haskell}
  inj :: Lin vars -Lin' vars
  inj (Var v) = Var' v
  inj (App s t1 t2) = App' s (inj t1) (inj t2)
  inj (Abs f) = Abs' (\v -inj $ f v)
~~~

The `elim` method uses a helper method `elim'` which runs in the `VarSupply` monad to generate new variable names. Every time an abstraction is found, a new variable name is generated, used to create a subexpression, and then eliminated from the subexpression.

~~~{.haskell}
  elim' :: Lin' vars -> VarSupply (Lin' vars)
  elim' (App' s t1 t2) = do
    t1' <- elim' t1
    t2' <- elim' t2
    return $ App' s t1' t2'
  elim' (Abs' f) = do
    name <- nextVar
    elim'' name (f name)
  elim' x = return $ x
~~~

The key function is `elim''` which eliminates a specific variable from a subexpression. Again, `elim''` runs in the `VarSupply` monad.

~~~{.haskell}
  elim'' :: Int -> Lin' (Int ': vars) -> VarSupply (Lin' vars)
~~~

A variable can be eliminated using the `I` combinator. Note that it is not necessary to check that names are equal here, because linearity means that if we were to encounter a different variable here, we would have taken a different path at an application further up the expression tree.

~~~{.haskell}
  elim'' name (Var' _) = return I
~~~

In the case of an application, our choices are limited by the types of the subexpression. The splitting context tells us how to proceed. If the eliminated variable occurs on the left of the application, we use the combinator `C`. If it occurs on the right, we use the combinator `B`. In either case, we only need to eliminate the variable recursively on one side of the application:

~~~{.haskell}
  elim'' name (App' (L s) t1 t2) = do
    t1' <- elim'' name t1
    return $ App' s (App' (trivial t1') C t1') t2
  elim'' name (App' (R s) t1 t2) = do
    t2' <- elim'' name t2
    return $ App' s (App' (trivial t1) B t1) t2'
~~~

In the case of a nested abstraction, we generate a new variable name, and then eliminate both variables in turn:

~~~{.haskell}
  elim'' name (Abs' f) = do
    name' <- nextVar
    tmp <- elim'' name (f name)
    elim'' name' tmp
~~~

Now we can test the eliminate method in ghci:

~~~
-- ghci> elim i
-- I
--
-- ghci> elim b
-- ((C ((B B) ((B B) I))) ((C ((B B) I)) I))
--
-- ghci> elim c
-- ((C ((B B) ((B C) ((C ((B B) I)) I)))) I)
~~~

These expressions can be written using the combinators `b'`, `c'` and `id`, and we can verify that the types of the eliminated terms are correct:

~~~
-- ghci> :t ((c' ((b' b') ((b' b') id))) ((c' ((b' b') id)) id))
-- (t1 -> t) -> (t2 -> t1) -> t2 -> t
-- 
-- ghci> :t ((c' ((b' b') ((b' c') ((c' ((b' b') id)) id)))) id)
-- (t2 -> t1 -> t) -> t1 -> t2 -> t
~~~
