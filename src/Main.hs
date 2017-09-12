{-# LANGUAGE DeriveFunctor #-}

module Main where

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
  | Compound [Variable] Expr Env

instance Show Proc where
  show (ProcPrim p) = show p
  show (Compound _ _ _) = "Procedure"

data PrimFunc
  = PFPlus
  | PFCons
  | PFCar
  | PFCdr
  | PFList
  | PFSymbolCheck
  | PFPairCheck
  | PFNullCheck
  | PFSet
  | PFEq
  | PFError
  deriving (Show, Eq)

data Env = Env [(Variable, LispObj)]

data ExprF a 
  = Obj LispObj
  | Var Variable
  -- | Quoted a
  | Assign Variable a a
  | Def Variable [Variable] a a
  | If a a a
  | Lambda [Variable] a
  -- | Begin a
  -- | Cond a
  | App a [a]
  deriving (Show, Functor)

showExprF :: Algebra ExprF String
showExprF e = case e of
  Var (Variable s) -> show s
  Obj p -> show p
  If _ _ _ -> "If expression"
  _ -> "OTHER"

showExpr :: Expr -> String
showExpr = cata showExprF

newtype LispError = LispError String 
  deriving Show

type Ret a = Either LispError a

test :: Expr
test = Fx $ Def (Variable "x") [] (Fx $ Obj (PrimInt 2))
      (Fx $ App (Fx $ Obj $ ProcObj $ ProcPrim PFPlus) [Fx $ Var (Variable "x"), Fx $ Obj (PrimInt 1)])

test2 :: Expr
test2 = Fx $ Def (Variable "x") [] (Fx $ Obj (PrimInt 20))
        (Fx $ Var $ Variable "x")

testBasic :: Expr
testBasic = Fx $ App (Fx $ Obj $ ProcObj $ ProcPrim PFPlus) [Fx $ Obj (PrimInt 2), Fx $ Obj (PrimInt 1)]

testTrivial :: Expr
testTrivial = (Fx $ Obj $ PrimInt 2)

emptyEnv :: Env
emptyEnv = Env []

eval :: Env -> Expr -> Ret LispObj
eval env (Fx expr) = case expr of
  (Obj o)                              -> Right o
  (Def var [] val next)                -> evalAssignment env var val >>= \x -> eval x next
  (App (Fx (Obj (ProcObj proc))) args) -> mapM (eval env) args >>= \xs -> apply proc xs
  (Var v)                              -> lookupVar env v
  x -> Left $ LispError $ "Error " ++ (showExpr $ Fx x)

evalAssignment :: Env -> Variable -> Expr -> Ret Env
evalAssignment env@(Env u) v e = do
  x <- eval env e
  return $ Env $ (v,x):u

apply :: Proc -> [LispObj] -> Ret LispObj
apply (ProcPrim prim) ps = applyPrim prim ps
apply _ _ = undefined

applyPrim :: PrimFunc -> [LispObj] -> Ret LispObj
applyPrim PFPlus [(PrimInt n), (PrimInt m)]
  = Right $ PrimInt (n + m)
applyPrim PFPlus _
  = Left $ LispError $ "(+) expects two ints"

evalIf :: Env -> Expr -> Expr -> Expr -> Ret LispObj
evalIf env cond true false = do
  x <- eval env cond
  case x of
    PrimBool b -> eval env (if b then true else false)
    y          -> Left $ LispError $ "Expected bool, got: " ++ show y

lookupVar :: Env -> Variable -> Ret LispObj
lookupVar (Env u) v
  = maybe (Left (LispError $ "Variable not defined " ++ show v)) Right (lookup v u)

main :: IO ()
main = putStrLn "yo"
