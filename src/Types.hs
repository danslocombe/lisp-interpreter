{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import Data.List (intersperse)

newtype Fix f = Fx (f (Fix f))
unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x

type Expr = Fix ExprF
type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> (Fix f -> a)
cata alg = alg . fmap (cata alg) . unFix

newtype Variable = Variable String deriving (Show, Eq)

data LispObj 
  = Nil
  | Cons LispObj LispObj
  | PrimInt Int
  | PrimBool Bool
  | SymbObj Symbol
  | ProcObj Proc
  deriving (Show)

data Symbol
  = List [Symbol]
  | SymbolString String
  deriving (Show)

data Proc
  = ProcPrim PrimFunc
  | ProcLisp [Variable] Expr Env

instance Show Proc where
  show (ProcPrim p) = show p
  show (ProcLisp args _ _) = "(" ++ concat (intersperse " " (map show args)) ++ ")"

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
  -- | PFEq
  | PFError
  deriving (Show, Eq)

data Env = Env [(Variable, LispObj)]

data ExprF a 
  = Obj LispObj
  | Var Variable
  | Assign Variable a a
  | Def Variable [Variable] a a
  | If a a a
  | Lambda [Variable] a
  -- | Cond a
  | App a [a]
  deriving (Show, Functor)

instance Show Expr where
  show = showExpr

showExprF :: Algebra ExprF String
showExprF e = case e of
  Var v -> show v
  Obj p -> show p
  If _ _ _ -> "If expression"
  Def v [] val next -> show v ++ " = " ++ show val ++ " in " ++ next
  App x args -> "( " ++ show x ++ concat (intersperse " " (map show args)) ++ ")"
  _ -> "OTHER EXPR"

showExpr :: Expr -> String
showExpr = cata showExprF
