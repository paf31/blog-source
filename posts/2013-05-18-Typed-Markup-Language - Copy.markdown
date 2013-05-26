---
title: A Typed Markup Language Based On Haskell
author: Phil Freeman
date: 2013/05/18
description: A simple markup language inspired by Haskell, including a type checker and parser.
tags: Haskell
---

In this post, I'm going to implement a simple markup language including a parser and type checker. The language's syntax will be based on Haskell's syntax, and the type system will look a little bit like System F.

Why am I interested in implementing such a markup language? Well, markup languages like JSON are good at describing the values in a dataset, but not their types, and XML extends this by allowing you to specify a schema, which corresponds to a set of typing rules. Languages like XAML allow you to extend this idea even futher by using an actual type system as a sort of global schema, and then extending the expressiveness of the language with things like subclass polymorphism.

In my opinion, XAML is great for quickly bootstrapping a DSL without having to worry about parsing, but the lack of generics makes it impossible to encode some simple constraints in the schema. I often find myself wanting a markup language which looks like Haskell and which uses similar typing rules.

The simplicity of the language below allows for a very simple implementation: typing is a simple bottom-up pass over a term.

With that, let's include some modules:

~~~{.haskell}
module Markup where

import Text.Parsec
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Language as L
import qualified Text.Parsec.Expr as E
import Data.Char
import Data.List (groupBy, nub, (\\))
import Data.Function (on)
import Control.Arrow ((***))
import Control.Applicative ((<$>), (<*>))
import Control.Monad (guard, zipWithM)
import qualified Control.Monad.State as S
~~~

Language Definition and Examples
---

I would like separate languages for type declarations and values. Here are some examples of the sorts of things I would like to be able to write (note, not all of these are expected to pass the typechecker):

~~~{.haskell}
exampleSchema = unlines 
  [ "Nil :: List Z a;"
  , "Singleton :: a -> List (S Z) a;"
  , "Cons :: a -> List n a -> List (S n) a;"
  , "Reverse :: List n a -> List n a;"
  , "Zip :: List n a -> List n b -> List n (Pair a b);"
  , "Tip :: Tree Z a;"
  , "Branch :: Tree n a -> a -> Tree n a -> Tree (S n) a" ]
  
example1 = "x = Zip (Cons 1 $ Cons 2 Nil) (Cons 1 Nil)" 
example2 = "x = Zip (Cons 1 $ Reverse $ Cons 2 $ Cons 3 Nil) (Cons \"a\" $ Cons \"b\" $ Singleton \"c\")" 
example3 = "x = Branch (Branch Tip 1 Tip) 2 (Branch Tip 3 Tip)" 
example4 = "x = Branch (Branch Tip 1 Tip) 2 Tip" 
example5 = "x = Cons (Branch Tip 1 Tip) Nil" 
example6 = "x = 1 `Cons` (2 `Cons` (3 `Cons` Nil))" 
example7 = "x = 1 `Cons` Nil `Zip` (2 `Cons` Nil)" 
example8 = "x = Nil `Cons` Nil `Zip` ((Cons 1 $ Nil) `Cons` Nil)" 
example9 = unlines 
  [ "x = Nil;"
  , "y = Cons x Nil;"
  , "z = Cons y Nil" ]
~~~

Notice that the type definition language supports things like type constructors and phantom types to allow me to encode simple constraints in the type of a declaration. For example, I can enforce the fact that the Zip constructor takes two lists of the same length. Also, there are no higher order functions, since there are no user-defined functions other than constructors, and no data declarations: all types are open for extension.

The value language copies several ideas from Haskell: infix constructors using backticks, and application using both juxtaposition and `$`. Other than that, the syntax is very simple.

Here are the data types which represent terms and types, along with their pretty printing functions:

~~~{.haskell}
data Tm 
  = TmName String
  | TmAtom String 
  | TmStr String 
  | TmInt Integer 
  | TmApp Tm Tm deriving (Show, Eq)

prettyTm :: Tm -> String
prettyTm (TmName s) = s
prettyTm (TmAtom s) = s
prettyTm (TmStr s) = show s
prettyTm (TmInt n) = show n
prettyTm (TmApp t1 t2@(TmApp _ _)) = prettyTm t1 ++ " (" ++ prettyTm t2 ++ ")"
prettyTm (TmApp t1 t2) = prettyTm t1 ++ " " ++ prettyTm t2
    
data Ty
  = TyCon String
  | TyApp Ty Ty
  | TyArr Ty Ty
  | TyVar String deriving (Show, Eq)
  
prettyTy :: Ty -> String
prettyTy (TyCon s) = s
prettyTy (TyVar v) = v
prettyTy (TyApp t1 t2@(TyCon _)) = prettyTy t1 ++ " " ++ prettyTy t2
prettyTy (TyApp t1 t2) = prettyTy t1 ++ " (" ++ prettyTy t2 ++ ")"
prettyTy (TyArr t1 t2) = prettyTy t1 ++ " -> (" ++ prettyTy t2 ++ ")"
~~~

Parsing Types and Values Using Parsec
---

Now let's implement a parser. To keep things simple, I'm going to use the lexer generator from `Text.Parsec.Token`.

~~~{.haskell}
languageDef :: P.LanguageDef st
languageDef = L.haskellStyle 
  { P.opStart         = fail "Operators are not supported"
  , P.opLetter        = fail "Operators are not supported"
  , P.reservedNames   = []
  , P.reservedOpNames = []
  , P.caseSensitive   = True }
  
lexer             = P.makeTokenParser languageDef 
identifier        = P.identifier lexer
whiteSpace        = P.whiteSpace lexer
stringLiteral     = P.stringLiteral lexer 
integer           = P.integer lexer 
symbol            = P.symbol lexer 
lexeme            = P.lexeme lexer 
parens            = P.parens lexer 
semi              = P.semi lexer 

identu :: Parsec String u String
identu = lookAhead upper >> identifier
  
identl :: Parsec String u String
identl = lookAhead lower >> identifier
~~~

There are parsers for terms:

~~~{.haskell}  
tmName :: Parsec String u Tm
tmName = lexeme $ identl >>= return . TmName
  
tmAtom :: Parsec String u Tm
tmAtom = lexeme $ identu >>= return . TmAtom

tmStr :: Parsec String u Tm
tmStr = lexeme $ stringLiteral >>= return . TmStr
  
tmInt :: Parsec String u Tm
tmInt = lexeme $ integer >>= return . TmInt

tmBracket :: Parsec String u Tm
tmBracket = lexeme $ parens tm
  
tm :: Parsec String u Tm
tm = E.buildExpressionParser 
     [ [ E.Infix (return TmApp) E.AssocLeft ]
	 , [ E.Infix (((.) TmApp . TmApp) <$> between (symbol "`") (symbol "`") tmAtom) E.AssocLeft ]
     , [ E.Infix (symbol "$" >> return TmApp) E.AssocRight ]  ]
     (tmBracket <|> tmStr <|> tmInt <|> try tmName <|> tmAtom)
~~~

and parsers for types:

~~~{.haskell}  		 
tyVar :: Parsec String u Ty
tyVar = lexeme $ identl >>= return . TyVar

tyCon :: Parsec String u Ty
tyCon = lexeme $ identu >>= return . TyCon
  
tyBracket :: Parsec String u Ty
tyBracket = parens ty
  
ty :: Parsec String u Ty
ty = E.buildExpressionParser 
     [ [ E.Infix (return TyApp) E.AssocLeft ]
     , [ E.Infix (symbol "->" >> return TyArr) E.AssocRight ] ]
     (tyBracket <|> try tyVar <|> tyCon)
~~~

The parsers `tm` and `ty` are then used to construct top-level parsers `schema` for type declarations and `doc` for value declarations:

~~~{.haskell}  	
valueDecl :: Parsec String u (String, Tm)
valueDecl = do
  name <- identl
  symbol "="
  t <- tm
  return (name, t)

doc :: Parsec String u [(String, Tm)]
doc = do
  whiteSpace 
  ds <- sepBy valueDecl (lexeme semi)
  eof
  return ds

typeDecl :: Parsec String u (String, Ty)
typeDecl = do
  name <- identu
  symbol "::"
  t <- ty
  return (name, t)

schema :: Parsec String u [(String, Ty)]
schema = do
  whiteSpace 
  ds <- sepBy typeDecl (lexeme semi)
  eof
  return ds
~~~

There is quite a bit going on here, but for the most part, the parser is assembled from reusable pieces in `Text.Parsec.Token` and some utility functions such as `sepBy`. The parser serves as a fairly good definition of the language syntax: the `tm` and `ty` parsers are just constructed using `buildExpressionParser` from the precedence tables for the term and type constructors respectively.

Implementing a Typechecker
---

The next step is to write the typechecker. This is actually quite simple since there are no function types other than constructors, and all quantification over free type variables implicitly happens at the top level.

The first step is to implement a unification function. This is very easy: types are either equal modulo variable substitution, or they do not unify at all:

~~~{.haskell}
unify :: Ty -> Ty -> Either String [(String, Ty)]
unify (TyVar s) (TyVar t) = return []
unify (TyVar s) t = return [(s, t)]
unify t (TyVar s) = return [(s, t)]
unify (TyCon ct1) (TyCon ct2) 
  | ct1 == ct2 = return []
unify (TyApp ty1 ty2) (TyApp ty3 ty4) = (++) <$> unify ty1 ty3 <*> unify ty2 ty4
unify (TyArr ty1 ty2) (TyArr ty3 ty4) = (++) <$> unify ty1 ty3 <*> unify ty2 ty4
unify t1 t2 = Left $ "Cannot unify " ++ prettyTy t1 ++ " with " ++ prettyTy t2
~~~

Type variable substitution is also a simple pattern match:

~~~{.haskell}
subst :: [(String, Ty)] -> Ty -> Ty
subst vs v@(TyVar s) = maybe v id $ lookup s vs
subst vs (TyApp t1 t2) = TyApp (subst vs t1) (subst vs t2)
subst vs (TyArr t1 t2) = TyArr (subst vs t1) (subst vs t2)
subst _ t = t
~~~

One important function which is used in the typechecker is `specialize`, which is used to apply a polymorphic function to an argument. The idea is that `specialize f x` should return the most general type of `f x`. The helper function `isFunction` is used to ensure that each type variable was unified with at most one type.

~~~{.haskell}
isFunction :: (Eq dom, Eq cod) => [(dom, cod)] -> Bool
isFunction = all ((== 1) . length . nub . map snd) . groupBy ((==) `on` fst)
  
specialize :: Ty -> Ty -> Either String Ty
specialize (TyArr t1 t2) t3 = do
  vs <- unify t1 t3
  guard $ isFunction vs
  return $ subst vs t2
specialize t _ = Left $ "Expected a function, found " ++ prettyTy t
~~~

We need one more helper function, which is used to rename free variables in a polymorphic type. This is used to make sure type variables do not overlap when a function application gets typed.

The `renameAll` function gives new names to all of the free type variables appearing in a type, using an association list to store the currently-generated names:

~~~{.haskell}
renameAll :: (Eq a) => a -> Ty -> S.State [((String, a), String)] Ty
renameAll _ t@(TyCon _) = return t
renameAll key (TyVar v) = do
  m <- S.get
  case lookup (v, key) m of
    Nothing -> do
	  let name = unusedName (map snd m)
	  S.modify $ (:) ((v, key), name)
	  return $ TyVar name
    Just name -> return $ TyVar name
renameAll key (TyApp t1 t2) = TyApp <$> renameAll key t1 <*> renameAll key t2
renameAll key (TyArr t1 t2) = TyArr <$> renameAll key t1 <*> renameAll key t2

unusedName :: [String] -> String
unusedName used = head $ map (("t" ++) . show) [0..] \\ used
~~~

We can now write the typechecker. It is a simple application of the `specialize` and `renameAll` helper functions:

~~~{.haskell}
typeOf :: [(String, Ty)] -> Tm -> Either String Ty
typeOf ctx (TmName s) = maybe (Left $ "Unknown name " ++ s) Right $ lookup s ctx
typeOf ctx (TmAtom s) = maybe (Left $ "Unknown constructor " ++ s) Right $ lookup s ctx
typeOf _ (TmInt _) = return (TyCon "Int")
typeOf _ (TmStr _) = return (TyCon "String")
typeOf ctx (TmApp t1 t2) = do
  tyArg <- typeOf ctx t2
  tyFun <- typeOf ctx t1
  flip S.evalState [] $ do 
    tyFun' <- renameAll True tyFun
    tyArg' <- renameAll False tyArg
    return $ specialize tyFun' tyArg'
  
typesOf :: [(String, Ty)] -> [(String, Tm)] -> Either String [(String, Ty)]
typesOf _ [] = return []
typesOf ctx ((name, tm):tms) = do
  ty <- typeOf  ctx tm
  let first = (name, ty)
  rest <- typesOf (first:ctx) tms
  return (first:rest)
~~~

Let's test the typechecker and parser on some of the examples at the top of the post:

~~~{.haskell}
parseErr p = either (Left . show) Right . parse p ""
  
test sch val = do
  s <- parseErr schema sch
  d <- parseErr doc val
  tys <- typesOf s d
  return $ map (id *** prettyTy) tys
~~~

    ghci> test exampleSchema example1
    Left "Cannot unify S Z with Z"
    ghci> test exampleSchema example2
    Right [("x","List (S (S (S Z))) (Pair Int String)")]
    ghci> test exampleSchema example3
    Right [("x","Tree (S (S Z)) Int")]
    
This is good - the ill-typed expressions returned a typing error, and the well-typed expressions returned the correct types, including the correctly inferred placeholders for phantom types.

There are a few other small additions to the language that I would like to make such as infix operators, kind checking, better error messages and support for other primitive types. 

A Mini Language For Relational Queries
---

As a more useful example, I've put together a minimal DSL for partially typed relational queries. The type system is used to make sure pairs of columns equated in joins have the same type, and to track the tables used in a query. Try replacing some of the constructors to see the ways in which the type system is used!

~~~{.haskell}
relationSchema = unlines 
  [ "UserID         :: Column User PK;"
  , "UserName       :: Column User String;"
  , "UserCompanyID  :: Column User PK;"
  , "CompanyID      :: Column Company PK;"
  , "CompanyName    :: Column Company String;"
  , "Table          :: HList (Column t) -> Query (Table t);"
  , "Join           :: Query r1 -> Query r2 -> Expr (Cross r1 r2) Bool -> Query (Cross r1 r2);"
  , "Column         :: Column t ty -> Expr (Table t) ty;"
  , "Left           :: Expr t1 ty -> Expr (Cross t1 t2) ty;"
  , "Right          :: Expr t2 ty -> Expr (Cross t1 t2) ty;"
  , "Eq             :: Expr t ty -> Expr t ty -> Expr t Bool;"
  , "Nil            :: HList f;"
  , "Cons           :: f t -> HList f -> HList f" ]

relationExample = unlines 
  [ "query = Join"
  , "          (Table (Cons UserName $ Cons UserCompanyID Nil))"
  , "          (Table (Cons CompanyID $ Cons CompanyName Nil))"
  , "          ((Left $ Column UserCompanyID) `Eq` (Right $ Column CompanyID))" ]
~~~

    ghci> test relationSchema relationExample
    Right [("query","Query (Cross (Table User) (Table Company))")]

Implementing a Kind Checker
---

Ensuring terms are well-typed is pretty useless if the type language is itself typeless, so let's implement a kind checker for type definitions in a schema.

The kind checker below is a modified version of a type checker from an earlier post. It works by generating kind constraints by performing a bottom-up traversal of a type, and then solving tose constraints by substitution.

Here is the type of kinds, including placeholders for unknown kinds:

~~~{.haskell}
type Unknown = Int
  
data Kind
  = KindUnknown Unknown
  | KindStar
  | KindArr Kind Kind deriving (Show, Eq)
  
prettyKind :: Kind -> String
prettyKind (KindUnknown n) = "k" ++ show n
prettyKind KindStar = "*"
prettyKind (KindArr k1@(KindArr _ _) k2) = "(" ++ prettyKind k1 ++ ") -> " ++ prettyKind k2
prettyKind (KindArr k1 k2) = prettyKind k1 ++ " -> " ++ prettyKind k2
~~~  

I'll start as usual with some basic utility functions which will be used during kind checking. Two crucial functions are the replacement of an unknown with another kind, and an occurs-check function:

~~~{.haskell}
replace :: Unknown -> Kind -> Kind -> Kind
replace i x u@(KindUnknown j) 
  | i == j = x
  | otherwise = u
replace _ _ KindStar = KindStar
replace i x (KindArr k1 k2) = KindArr (replace i x k1) (replace i x k2)

occursCheck :: Unknown -> Kind -> Either String ()
occursCheck i (KindUnknown j) | i == j = return ()
occursCheck i x = occursCheck' i x where
  occursCheck' i (KindUnknown j) | i == j = fail "Occurs check failed during kind checking"
  occursCheck' i (KindArr k1 k2) = occursCheck' i k1 >> occursCheck' i k2
  occursCheck' _ _ = return ()
~~~

Constraint generation happens by unification. The `unify` function takes two kinds and generates the constraints imposed by their equality, or returns a unification error.

~~~{.haskell}
type KindConstraint = (Unknown, Kind)

type SolutionSet = Unknown -> Kind

unifyKinds :: Kind -> Kind -> Either String [KindConstraint]
unifyKinds (KindUnknown i) x = return [(i, x)]
unifyKinds x (KindUnknown i) = return [(i, x)]
unifyKinds KindStar KindStar = return  []
unifyKinds (KindArr k1 k2) (KindArr k3 k4) = (++) <$> unifyKinds k1 k3 <*> unifyKinds k2 k4
unifyKinds k1 k2 = fail $ "Cannot unify " ++ show k1 ++ " with " ++ show k2
~~~

The general constraint generation function works bottom-up over the structure of a type:

~~~{.haskell}
generateConstraints :: (Monad m, S.MonadState ([(String, Unknown)], Unknown) m) => Ty -> m [KindConstraint]
generateConstraints ty = do
  (cs, n) <- generateConstraints' ty
  return $ (n, KindStar) : cs
  where
  generateConstraints' :: (Monad m, S.MonadState ([(String, Unknown)], Unknown) m) => Ty -> m ([KindConstraint], Unknown)
  generateConstraints' (TyVar v) = do
    (m, _) <- S.get
    case lookup v m of
      Nothing -> do
        n <- newUnknown
        S.modify $ (:) (v, n) *** id
        return ([], n)
      Just unk -> return ([], unk)
  generateConstraints' (TyCon s) = do
    (m, _) <- S.get
    case lookup s m of
      Nothing -> do
        n <- newUnknown
        S.modify $ (:) (s, n) *** id
        return ([], n)
      Just unk -> return ([], unk)
  generateConstraints' (TyApp ty1 ty2) = do
    (c1, n1) <- generateConstraints' ty1
    (c2, n2) <- generateConstraints' ty2
    thisName <- newUnknown
    let newConstraint = (n1, KindArr (KindUnknown n2) (KindUnknown thisName))
    return $ ((newConstraint:c1) ++ c2, thisName)
    
newUnknown :: (Monad m, S.MonadState (a, Unknown) m) => m Unknown
newUnknown = S.modify (id *** (+1)) >> S.get >>= return . snd
~~~

Constraints are solved by substitution using `replace`:
  
~~~{.haskell}
solve :: [KindConstraint] -> Either String SolutionSet
solve cs = solve' cs KindUnknown where
  solve' [] ss = return ss
  solve' (c@(i, x):cs) ss = do
    occursCheck i x
    cs' <- substConstraints c cs
    let ss' = substSolutionSet c ss
    solve' cs' ss'
  substConstraints c [] = return []
  substConstraints (i, x) ((j, y): cs) 
    | i == j = (++) <$> substConstraints (i, x) cs <*> unifyKinds x y
    | otherwise = (:) (j, replace i x y) <$> substConstraints (i, x) cs
  substSolutionSet (i, x) = (.) (replace i x)
~~~

Now we can write the kind checker. The `applications` function splits an arrow type into parts, so that kind checking can work on each part.

~~~{.haskell}
applications :: Ty -> [Ty]
applications t = applications' t [] where
  applications' (TyArr t1 t2) = applications' t1 . applications' t2
  applications' t = (:) t

kindCheck :: [Ty] -> Either String [(String, Kind)]
kindCheck = uncurry kindCheck' 
  . (solve *** id) 
  . (concat *** fst) 
  . flip S.runState ([], 0) 
  . flip forM generateConstraints 
  . concatMap applications
  where
  kindCheck' :: Either String SolutionSet -> [(String, Unknown)] -> Either String [(String, Kind)]
  kindCheck' ss unks = do
    ss' <- ss
    return $ map (id *** ss') unks
~~~

Let's try it out.

    ghci> parseErr schema exampleSchema >>= fmap (map (id *** prettyKind)) . kindCheck . map snd
    Right [
      ("Tree", "k10 -> * -> *"),
      ("Pair", "* -> * -> *"),
      ("b", "*"),
      ("n", "k10"),
      ("S", "k10 -> k10"),
      ("a", "*"),
      ("Z", "k10"),
      ("List", "k10 -> * -> *") ]
