module Main where

import Types
import Parse

import Text.Parsec
import Text.Parsec.Char
import Data.Either (isRight)

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
  (Def var args body next)             -> evalAssignmentFunc env var args body >>= \x -> eval x next
  (App func args) -> do
     f <- eval env func 
     args <- mapM (eval env) args 
     case f of
       ProcObj proc -> apply proc args
       _ -> Left $ LispError $ 
            "Error tried to apply " ++ show f ++
            " which is not a function"
  (Var v)                              -> lookupVar env v
  x                                    -> Left $ LispError $ "Error " ++ (showExpr $ Fx x)

evalAssignment :: Env -> Variable -> Expr -> Ret Env
evalAssignment env@(Env u) v e = do
  x <- eval env e
  return $ Env $ (v,x):u

evalAssignmentFunc :: Env -> Variable -> [Variable] -> Expr -> Ret Env
evalAssignmentFunc env@(Env u) name args body = do
  let proc = ProcLisp args body env
  return $ Env $ (name, ProcObj proc):u
  

apply :: Proc -> [LispObj] -> Ret LispObj
apply (ProcPrim prim) ps = applyPrim prim ps
apply (ProcLisp args body (Env env)) ps
  = let env' =  Env $ (zip args ps) ++ env
    in eval env' body

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

testSource :: String
testSource = "(define x 2)\n(+ x 2)"

dothing :: String -> IO ()
dothing s = 
  let x = runParser parseOverall "" "" s
  in case x of
    Right parse -> case eval emptyEnv parse of
      Right ran -> putStrLn $ show ran
      Left err -> putStrLn $ "Runtime error: " ++ show err
    Left err -> putStrLn $ show err
    
main :: IO ()
main = putStrLn "yo"
