module Main where

import Types
import Parse

import Text.Parsec
import Text.Parsec.Char
import Data.Either (isRight)
import Control.Monad (forever)

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
  (If predicate e1 e2)                 -> evalIf env predicate e1 e2
  (Var v)                              -> lookupVar env v
  x                                    -> Left $ LispError $ "Error " ++ (showExpr $ Fx x)

evalAssignment :: Env -> Variable -> Expr -> Ret Env
evalAssignment env@(Env u) v e = do
  x <- eval env e
  return $ Env $ (v,x):u

evalAssignmentFunc :: Env -> Variable -> [Variable] -> Expr -> Ret Env
evalAssignmentFunc env@(Env u) name args body = do
  let proc = ProcLisp args body env'
      env' = Env $ (name, ProcObj proc):u
  return $ env'
  

apply :: Proc -> [LispObj] -> Ret LispObj
apply (ProcPrim prim) ps = applyPrim prim ps
apply (ProcLisp args body (Env env)) ps
  = let env' =  Env ((zip args ps) ++ env)
    in eval env' body

applyPrim :: PrimFunc -> [LispObj] -> Ret LispObj
applyPrim PFPlus [(PrimInt n), (PrimInt m)]
  = Right $ PrimInt (n + m)
applyPrim PFPlus _
  = Left $ LispError $ "(+) expects two ints"
applyPrim PFMinus [(PrimInt n), (PrimInt m)]
  = Right $ PrimInt (n - m)
applyPrim PFMinus _
  = Left $ LispError $ "(-) expects two ints"
applyPrim PFLess [(PrimInt n), (PrimInt m)]
  = Right $ PrimBool (n < m)
applyPrim PFLess _
  = Left $ LispError $ "(<) expects two ints"
applyPrim PFGreater [(PrimInt n), (PrimInt m)]
  = Right $ PrimBool (n > m)
applyPrim PFGreater _
  = Left $ LispError $ "(>) expects two ints"
applyPrim PFEq [(PrimInt n), (PrimInt m)]
  = Right $ PrimBool (n == m)
applyPrim PFEq _
  = Left $ LispError $ "(=) expects two ints"
applyPrim PFCons [x, y]
  = Right $ Cons x y
applyPrim PFCons _
  = Left $ LispError $ "(cons) expects two arguments"
applyPrim PFCar [(Cons x y)]
  = Right $ x
applyPrim PFCar _
  = Left $ LispError $ "(car) expects one argument of type (cons x y)"
applyPrim PFCdr [(Cons x y)]
  = Right $ y
applyPrim PFCdr _
  = Left $ LispError $ "(cdr) expects one argument of type (cons x y)"
applyPrim PFNullCheck [Nil]
  = Right $ PrimBool True
applyPrim PFNullCheck [x]
  = Right $ PrimBool False
applyPrim PFNullCheck _
  = Left $ LispError $ "(null?) expects one argument of type"
applyPrim PFList xs
  = Right $ foldr Cons Nil xs


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

testRec :: String
testRec = "(define (odd x) (if (> x 1) then (odd (- x 2)) else (= x 1))) (odd 5)"

lispSourceMap :: String
lispSourceMap 
  = "(define (map f xs)"            ++ "\n" ++
      "(if (null? xs)"              ++ "\n" ++
         "then nil"                 ++ "\n" ++
         "else ("                   ++ "\n" ++
            "cons (f (car xs))"     ++ "\n" ++
                 "(map f (cdr xs))" ++ "\n" ++
        ")"                         ++ "\n" ++
      ")"                           ++ "\n" ++
    ")"

lispSourceMapUse = lispSourceMap    ++ "\n" ++
  " (define (double x) (+ x x)) "   ++ "\n" ++
  " (map double (list 1 2 3 4 5))"

lisp :: String -> IO ()
lisp s =
  let x = runParser parseLisp "" "" s
  in case x of
    Right parse -> case eval emptyEnv parse of
      Right ran -> putStrLn $ show ran
      Left err -> putStrLn $ "Runtime error: " ++ show err
    Left err -> putStrLn $ show err
    
main :: IO ()
main = forever (getLine >>= lisp) 
