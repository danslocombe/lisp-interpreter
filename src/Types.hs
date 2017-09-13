{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import Data.List (intersperse)

newtype Variable 
  = Variable String deriving (Show, Eq)

data LispObj 
  = Nil                  -- Nil object usually used to represent empty list
  | Cons LispObj LispObj -- Construct of two LispObjs
  | PrimInt Int          -- Primative integer type
  | PrimBool Bool        -- Primative boolean type
  | SymbObj Symbol       -- Symbol 
  | ProcObj Proc         -- Procedure
  deriving (Show)

data Symbol
  = List [Symbol]
  | SymbolString String
  deriving (Show)

-- A procedure is either a built-in primative or define in lisp
data Proc
  = ProcPrim PrimFunc
  | ProcLisp [Variable] Lisp Env

instance Show Proc where
  show (ProcPrim p) = show p
  show (ProcLisp args _ _) = "(" ++ concat (intersperse " " (map show args)) ++ ")"

-- Possible of primative procedures
data PrimFunc
  = PFPlus
  | PFMinus
  | PFLess
  | PFGreater
  | PFEq
  | PFCons
  | PFCar
  | PFCdr
  | PFList
  | PFSymbolCheck
  | PFPairCheck
  | PFNullCheck
  | PFSet
  | PFSymbolEq
  | PFError
  deriving (Show, Eq)

-- An environment is a list of mappings from variable to object
-- Lookup is done by returning the first item in the list, so
-- inner scoped variables override outer scoped ones
data Env = Env [(Variable, LispObj)] deriving (Show)

-- The lisp AST
data LispF a 
  = Obj LispObj
  | Var Variable
  | Assign Variable a a
  | Def Definition a
  | If a a a
  | Lambda [Variable] a
  | Cond [(a,a)]
  | App a [a]
  deriving (Show, Functor)

data Definition = Definition Variable [Variable] Lisp deriving (Show)

-- Represents the fix point of a data type
newtype Fix f = Fx (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x

type Algebra f a = f a -> a

-- A catamorphism is a generalization of a fold 
cata :: Functor f => Algebra f a -> (Fix f -> a)
cata alg = alg . fmap (cata alg) . unFix

type Lisp = Fix LispF

data LispRepl = LRDef Definition | LRObj Lisp deriving Show

instance Show Lisp where
  show = showLisp

showLispF :: Algebra LispF String
showLispF e = case e of
  Var v -> show v
  Obj p -> show p
  If _ _ _ -> "If expression"
  Def (Definition v [] val) next -> show v ++ " = " ++ show val ++ " in " ++ next
  App x args -> "( " ++ show x ++ concat (intersperse " " (map show args)) ++ ")"
  _ -> "OTHER EXPR"

showLisp :: Lisp -> String
showLisp = cata showLispF
