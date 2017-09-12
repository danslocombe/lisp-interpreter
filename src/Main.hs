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

newtype TypeError = TypeError String 
  deriving Show

type Ret a = Either TypeError a

-- test = Fx $ Def (Fx $ Variable "x") (Fx $ Obj (PrimInt 2))
        -- (Fx $ App (Fx $ LispObj (ProcPrim PFPlus)) (Fx $ Var (Variable "x")) (Fx $ Obj (PrimInt 1)))
        --
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

eval :: Env -> Expr -> Ret Expr
eval env (Fx expr) = case expr of
  (Obj o)                              -> Right . Fx . Obj $ o
  (Def var [] val next)                -> evalAssignment env var val >>= \x -> eval x next
  (App (Fx (Obj (ProcObj proc))) args) -> apply proc $ map (\(Fx (Obj o)) -> o) args
  (Var v)                              -> Fx . Obj <$> lookupVar env v
  x -> Left $ TypeError $ "Error " ++ (showExpr $ Fx x)

-- evalSequence :: Env -> [Expr] -> Ret (Env, Expr)
-- evalSequence env exps = foldM (eval) 

evalAssignment :: Env -> Variable -> Expr -> Ret Env
evalAssignment env@(Env u) v e =
  eval env e >>= (\x -> case x of
    Fx (Obj p) -> Right $ Env $ (v,p):u
    y            -> Left $ TypeError $ "Could not assign " ++ show v ++ " non-obj value"
  )

apply :: Proc -> [LispObj] -> Ret Expr
apply (ProcPrim prim) ps = Fx . Obj <$> applyPrim prim ps
apply _ _ = undefined

applyPrim :: PrimFunc -> [LispObj] -> Ret LispObj
applyPrim PFPlus [(PrimInt n), (PrimInt m)]
  = Right $ PrimInt (n + m)
applyPrim PFPlus _
  = Left $ TypeError $ "(+) expects two ints"

evalIf :: Env -> Expr -> Expr -> Expr -> Ret Expr
evalIf env cond true false = 
  eval env cond >>= (\x -> case x of
    Fx (Obj (PrimBool b)) -> eval env (if b then true else false)
    y          -> Left $ TypeError $ "Expected bool, got: " ++ showExpr (y)
  )

lookupVar :: Env -> Variable -> Ret LispObj
lookupVar (Env u) v
  = maybe (Left (TypeError $ "Variable not defined " ++ show v)) Right (lookup v u)

main :: IO ()
main = putStrLn "yo"
