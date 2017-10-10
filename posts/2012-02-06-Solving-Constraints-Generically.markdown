---
title: Solving Constraints Generically
author: Phil Freeman
date: 2012/02/06
description: Generalizing constraint solving algorithms over arbitrary term algebras.
tags: Haskell, Compilers
---

In this post, I\'d like to revisit Algorithm W, which I discussed when I wrote about Purity\'s typechecker.

Recalling the approach taken before, a term was typed by collecting constraints between unknown type variables by traversing the term in question, and then solving those constraints by substitution. This time I\'d like to generalize the second part of the algorithm, to solve constraints over any type functor by substitution.

~~~{.text}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Solver where
~~~

The free monad for a functor `f` describes "incomplete `f`-trees" with variables of type `a`, so it is not surprising that it appears in the type of our constraints:

~~~{.text}
data Free f a = Return a | Wrap (f (Free f a))

instance (Functor f) => Monad (Free f) where
  return = Return
  (Return x) >>= f = f x
  (Wrap xs) >>= f = Wrap (fmap (>>= f) xs)
~~~

Type variables are represented as integers:

~~~{.text}
type Unknown = Int
~~~

Then a constraint for a type functor `f` asserts equality between an unknown (identified by an integer) and an incomplete `f`-tree with variables in `Unknown`. It is represented as a pair:

~~~{.text}
type Constraint f = (Unknown, Free f Unknown)
~~~

A solution set is just a mapping from unknowns to incomplete `f`-trees:

~~~{.text}
type SolutionSet f = Unknown -> Free f Unknown
~~~

A functor is `Unifiable` if it defines types which can be unified to give constraints. It is expected that unknowns unify with all terms. For simplicity, I have used `error` below to indicate the failure of terms to `unify`, but one could rewrite `unify` to give values in some monad supporting errors.

~~~{.text}
class Unifiable f where
  unify :: Free f Unknown -> Free f Unknown -> [Constraint f]
~~~

The method replace takes a variable, and a new term, and substitutes all instances of that variable for the term, using the bind operation of the free monad:

~~~{.text}
replace :: (Functor f) => Unknown -> Free f Unknown -> Free f Unknown -> Free f Unknown
replace i x = flip (>>=) (\j -> if i == j then x else Return j)
~~~

`solve` takes a set of constraints and returns a solution set. It does so by repeatedly substituting the first constraint into the remaining constraints, and into the solution set, removing one unknown at a time. As it does so, it may be necessary to unify one or more types, generating further constraints. These are added to the queue of constraints still to be solved.

~~~{.text}
solve :: (Functor f, Unifiable f) => [Constraint f] -> SolutionSet f
solve cs = solve' cs Return where
  solve' [] ss = ss
  solve' (c:cs) ss =
    let cs' = substConstraints c cs in
    let ss' = substSolutionSet c ss in
    solve' cs' ss'
  substConstraints c [] = []
  substConstraints (i, x) ((j, y): cs)
    | i == j = (substConstraints (i, x) cs) ++ (unify x y)
    | otherwise = ((j, replace i x y) : substConstraints (i, x) cs)
  substSolutionSet (i, x) = (.) (replace i x)
~~~

Now for a small motivating example before moving onto gathering constraints. The following functor defines a single basic type of integers, and the type of arrows:

~~~{.text}
data Type a = IntType | Arrow a a

instance Show (Free Type Unknown) where
  show (Return i) = show i
  show (Wrap IntType) = "Int"
  show (Wrap (Arrow a b)) = "(" ++ show a ++ " -" ++ show b ++ ")"
~~~

Type defines a functor:

~~~{.text}
instance Functor Type where
  fmap f IntType = IntType
  fmap f (Arrow t1 t2) = Arrow (f t1) (f t2)
~~~

And types can be unified:

~~~{.text}
instance Unifiable Type where
  unify (Return i) x = [(i, x)]
  unify x (Return i) = [(i, x)]
  unify (Wrap IntType) (Wrap IntType) = []
  unify (Wrap (Arrow a b)) (Wrap (Arrow c d)) = (unify a c) ++ (unify b d)
  unify _ _ = error "Cannot unify"
~~~

The following example corresponds to the constraints gathered by Algorithm W during the typing of the `S` combinator:

~~~{.text}
example = [
  (0, Wrap (Arrow (Return 2) (Return 3))),
  (1, Wrap (Arrow (Return 2) (Return 4))),
  (3, Wrap (Arrow (Return 4) (Return 5))),
  (6, Wrap (Arrow (Return 2) (Return 5))),
  (7, Wrap (Arrow (Return 1) (Return 6))),
  (8, Wrap (Arrow (Return 0) (Return 7))) ]
~~~

One can check that solve does indeed determine the correct type for the `S` combinator:

~~~{.text}
ghci> solve example 8

((2 -> (4 -> 5)) -> ((2 -> 4) -> (2 -> 5)))
~~~

Now that we can solve constraints, let\'s write a method to determine constraints for the specific example of terms of the simply-typed lambda calculus. There are three types of terms: variables, applications and abstractions. To save passing around environments containing strings, we will represent the bound variables in a term using a type variable. Abstraction will introduce a new type variable, and parametricity will prevent us from creating any ill-defined terms:

~~~{.text}
data Term a = Var a | App (Term a) (Term a) | Abs (forall b. b -> Term (Either a b))
~~~

A closed term is a term with no variables, so we need a type with no inhabitants:

~~~{.text}
data Void

type Closed = Term Void
~~~

An environment is a mapping from variable names to type variables. The find method looks up a type variable in an environment:

~~~{.text}
newtype Env a = Env { find :: a -> Maybe Unknown }
~~~

The only environment for closed terms is the empty environment:

~~~{.text}
nil :: Env Void
nil = Env (\_ -> undefined)
~~~

To add to an environment, we require a new addend in the type variable of the environment:

~~~{.text}
add :: Env a -> Unknown -> Env (Either a b)
add e i = Env (either (find e) (const (Just i)))
~~~

During the constraint gathering process, we will need to generate names for unknowns. This state monad is used to generate unique type variable names during the constraint-gathering process:

~~~{.text}
newtype Context a = Context { runContext :: Unknown -> (a, Unknown) }

instance Monad Context where
  return a = Context (\i -> (a, i))
  f >>= g = Context (\i -> let (a, j) = runContext f i in runContext (g a) j)
~~~

And this action generates a new name:

~~~{.text}
next :: Context Unknown
next = Context (\i -> (i, i + 1))
~~~

Now that we have all of the required pieces, we can implement generateConstraints, which takes a term and generates a set of constraints. We can use the method above to solve for the types of all subterms. The method works by assigning names to all subterms in a bottom-up fashion, and adding constraints between those names where necessary.

~~~{.text}
generateConstraints :: Closed -> [Constraint Type]
generateConstraints t = let ((cs, _), _) = runContext (generateConstraints' nil t) 0 in cs where
  generateConstraints' :: Env a -> Term a -> Context ([Constraint Type], Unknown)
  generateConstraints' env (Var a) = do
    let (Just n) = find env a
    return ([], n)
  generateConstraints' env (App t1 t2) = do
    (c1, n1) <- generateConstraints' env t1
    (c2, n2) <- generateConstraints' env t2
    thisName <- next
    let newConstraint = (n1, Wrap (Arrow (Return n2) (Return thisName)))
    return ((newConstraint:c1) ++ c2, thisName)
  generateConstraints' env (Abs f) = do
    varName <- next
    let newenv = add env varName
    (cs, n) <- generateConstraints' newenv (f ())
    thisName <- next
    let newConstraint = (thisName, Wrap (Arrow (Return varName) (Return n)))
    return ((newConstraint:cs), thisName)
~~~

Finally, here are some more examples, the `S` and `K` combinators:

~~~{.text}
s = Abs (\x -> Abs (\y -> Abs (\z -> App (App (Var $ Left $ Left $ Right $ x) (Var $ Right $ z)) (App (Var $ Left $ Right $ y) (Var $ Right $ z)))))

k = Abs (\x -> Abs (\y -> Var $ Left $ Right $ x))
~~~

We can test these examples in GHCi as follows:

~~~{.text}
ghci> solve (generateConstraints s) 8

((2 -> (4 -> 5)) -> ((2 -> 4) -> (2 -> 5)))

ghci> solve (generateConstraints k) 3

(0 -> (1 -> 0))
~~~
