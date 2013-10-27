---
title: Pretty Printing Arrows
author: Phil Freeman
date: 2013/10/27
description:
tags: Haskell
---

I'd like to show a neat use of arrows for pretty printing an AST.

~~{.haskell}
{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-} 

import Data.Maybe (fromMaybe)
import Data.Function (fix)
import qualified Control.Category as C
import Control.Category ((>>>))
import qualified Control.Arrow as A
import Control.Arrow ((***), (<+>))
~~

Suppose you had defined a type of syntax trees and wanted to write a function to print their representations as code: 

~~{.haskell}
data Expr = Var String
          | Abs String Expr
          | App Expr Expr deriving Show
~~

## A First Attempt

A first attempt might look something like this:

~~{.haskell}
pretty1 :: Expr -> String
pretty1 (Var v) = v
pretty1 (Abs v e) = "\\" ++ v ++ " -> " ++ pretty1 e
pretty1 (App e1 e2) = "(" ++ pretty1 e1 ++ ") (" ++ pretty1 e2 ++ ")"
~~

This certainly generates valid code, but the resulting strings tend to contain a lot of redundant parentheses:

    ghci> let s = Abs "x" $ 
                    Abs "y" $ 
                      Abs "z" $ 
                        App (App (Var "x") (Var "z")) 
                              (App (Var "y") (Var "z"))
                              
    ghci> pretty1 s
    "\\x -> \\y -> \\z -> ((x) (z)) ((y) (z))"

## A Better Approach

Another approach is to thread the current precedence level as an argument, and to parenthesize as a last resort:

~~{.haskell}
type Precedence = Int

pretty2 :: Expr -> String
pretty2 = pretty2' 0
  where
  pretty2' :: Precedence -> Expr -> String
  pretty2' _ (Var v) = v
  pretty2' p (Abs v e) | p < 2 = "\\" ++ v ++ " -> " ++ pretty2' 0 e
  pretty2' p (App e1 e2) = pretty2' 1 e1 ++ " " ++ pretty2' p e2
  pretty2' _ e = "(" ++ pretty2' 0 e ++ ")"
~~

We can verify that this approach generates better code, and that the precedence rules are still respected:

    ghci> pretty2 s
    "\\x -> \\y -> \\z -> x z (y z)"
    
    ghci> let k = App (Abs "x" $ Var "x") (Abs "x" $ Var "x")
    
    ghci> pretty2 k
    "(\\x -> x) (\\x -> x)"

These approaches are fine, but as the complexity of the AST type increases, I find it harder to keep the various precedence relationships in mind.

## First Class Patterns

Arrows provide a way to express pattern matches as first class values in a simple way, and then to compose those patterns to create full pretty printers.

First class patterns and their use in pretty printing are not new ideas: interested readers might like to take a look at the following papers for more information:

- "Type-safe pattern combinators (Functional Pearl)" by Morten Rhiger
- "Invertible Syntax Descriptions: Unifying Parsing and Pretty Printing" by Tillmann Rendel and Klaus Ostermann

However, I think the use of arrows provides a novel way to build up patterns into complex pretty printers.

The really neat thing is that almost all of the required code can be derived using `GeneralizedNewtypeDeriving`!

Here is the definition of a `Pattern` as an `Arrow`. It takes a value of type `a`, and either matches successfully, returning a value of type `b`, or fails. Failure is modelled using the Kleisli category for the `Maybe` monad:

~~{.haskell}
newtype Pattern a b = Pattern { runPattern :: A.Kleisli Maybe a b } 
  deriving (C.Category, A.Arrow, A.ArrowZero, A.ArrowPlus)

pattern :: Pattern a b -> a -> Maybe b
pattern = A.runKleisli . runPattern
~~

We can derive instances for `Category`, `Arrow`, `ArrowZero`, and `ArrowPlus`. The intuition here is that `Category` gives us composition of patterns, i.e. nested patterns, `Arrow` gives combinators for working with patterns involving tuples, and `ArrowZero` and `ArrowPlus` give us a way to deal with failure and backtracking.

Note: there is also an instance for `Applicative` which gives another way to work with simultaneous patterns, but I won't write it out here.

One thing we can't immediately `derive` is the `Functor` instance for `Pattern`, which will come in useful later. Fortunately, it is easy to write by hand

~~{.haskell}
instance Functor (Pattern a) where
  fmap f p = Pattern $ A.Kleisli $ fmap f . pattern p
~~

## Some Simple Patterns

Here are some examples of `Pattern`s

~~{.haskell}
var :: Pattern Expr String
var = Pattern $ A.Kleisli var'
  where var' (Var s) = Just s
        var' _ = Nothing

lam :: Pattern Expr (String, Expr)
lam = Pattern $ A.Kleisli abs'
  where abs' (Abs s e) = Just (s, e)
        abs' _ = Nothing

app :: Pattern Expr (Expr, Expr)
app = Pattern $ A.Kleisli app'
  where app' (App e1 e2) = Just (e1, e2)
        app' _ = Nothing
~~

I imagine these are the sort of the thing one could write a Template Haskell splice for. They also seem quite similar to Prisms, which might provide another way to write this code.

## Combining Patterns

Now we can write some combinators in the spirit of `Text.Parsec` which allow us to build up new patterns from old, and to apply a pattern recursively:

~~{.haskell}
chainl :: Pattern a (a, a) -> (r -> r -> r) -> Pattern a r -> Pattern a r
chainl split f p = fix $ \c -> (split >>> ((c <+> p) *** p) >>> A.arr (uncurry f))

chainr :: Pattern a (a, a) -> (r -> r -> r) -> Pattern a r -> Pattern a r
chainr split f p = fix $ \c -> (split >>> (p *** (c <+> p)) >>> A.arr (uncurry f))

wrap :: Pattern a (s, a) -> (s -> r -> r) -> Pattern a r -> Pattern a r
wrap split f p = fix $ \c -> (split >>> (C.id *** (c <+> p)) >>> A.arr (uncurry f))
~~

## Precedence Tables

In fact, we can go one step futher and derive a pattern from a precedence table in the manner of `Text.Parsec.Expr`:

~~{.haskell}
data OperatorTable a r = OperatorTable { runOperatorTable :: [ [Operator a r] ] }

data Operator a r where
  AssocL :: Pattern a (a, a) -> (r -> r -> r) -> Operator a r
  AssocR :: Pattern a (a, a) -> (r -> r -> r) -> Operator a r
  Wrap   :: Pattern a (s, a) -> (s -> r -> r) -> Operator a r

buildPrettyPrinter :: OperatorTable a r -> Pattern a r -> Pattern a r
buildPrettyPrinter table p = foldl (\p' ops -> foldl1 (<+>) (flip map ops $ \op ->
  case op of
    AssocL pat g -> chainl pat g p'
    AssocR pat g -> chainr pat g p'
    Wrap pat g -> wrap pat g p'
  ) <+> p') p $ runOperatorTable table
~~

We need one final function, which parenthesizes an expression:

~~{.haskell}
parenthesize :: Pattern a String -> Pattern a String
parenthesize = fmap parens 
  where
  parens s = '(':s ++ ")"
~~

## Finally ...

This gives us the parts we need to express our previous pretty printer as a `Pattern`:

~~{.haskell}
expr = buildPrettyPrinter ops (var <+> parenthesize expr)
  where 
    ops = OperatorTable
      [ [ AssocL app $ \e1 e2 -> e1 ++ " " ++ e2 ]
      , [ Wrap lam $ \b s -> "\\" ++ b ++ " -> " ++ s ]
      ]
      
pattern3 :: Expr -> String
pattern3 = fromMaybe (error "Incomplete pattern match") . pattern expr
~~

Note that, just like when we define parsers using `Text.Parsec.Expr`, the use of combinators allows us to write code which directly represents the precedence table!

## Example - Integer Expressions and Binary Operations

Here's another example, of expressions supporting integer constants and binary operators.

~~{.haskell}
data Eqn = Const Int
         | Bin Eqn Char Eqn deriving Show

con :: Pattern Eqn Int
con = Pattern $ A.Kleisli con'
  where con' (Const n) = Just n
        con' _ = Nothing

bin :: Char -> Pattern Eqn (Eqn, Eqn)
bin c = Pattern $ A.Kleisli bin'
  where bin' (Bin e1 c' e2) | c == c' = Just (e1, e2)
        bin' _ = Nothing

eqn = buildPrettyPrinter ops (fmap show con <+> parenthesize eqn)
  where 
    ops = OperatorTable
      [ [ binOp '*', binOp '/' ]
      , [ binOp '+', binOp '-' ]
      ]
    binOp c = AssocL (bin c) $ \e1 e2 -> e1 ++ c : e2
~~

For yet another (more developed) example, see my current project on GitHub, [here](http://github.com/paf31/purescript/).
