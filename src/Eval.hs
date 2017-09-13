module Eval where

import Types

eval :: Env -> Lisp -> Ret LispObj
eval env (Fx expr) = case expr of

  (Obj o) -> Right o

  (Def d next) -> do
    x <- evalDefinition env d
    eval x next

  -- Application of a function
  (App func args) -> do
     -- Evaluate function and arguments to ListObjs
     f <- eval env func 
     args <- mapM (eval env) args 
     case f of
       ProcObj proc -> apply proc args
       _ -> Left $ LispError $ 
            "Error tried to apply " ++ show f ++
            " which is not a function"

  -- If condition
  (If predicate e1 e2) -> evalIf env predicate e1 e2

  -- Variable
  (Var v) -> lookupVar env v

  -- Other
  x -> Left $ LispError $ "Error " ++ (showLisp $ Fx x)


evalDefinition :: Env -> Definition -> Ret Env
  -- Definition of a value (has no parameters)
evalDefinition env (Definition var [] val) = evalAssignment env var val
  -- Definition of a function (has parameters)
evalDefinition env (Definition var args body) = evalAssignmentFunc env var args body

-- Evaluate declaration of value, updates environment
evalAssignment :: Env -> Variable -> Lisp -> Ret Env
evalAssignment env@(Env u) v e = do
  x <- eval env e
  return $ Env $ (v,x):u

-- Evaluate declaration of function, updates environment
evalAssignmentFunc :: Env -> Variable -> [Variable] -> Lisp -> Ret Env
evalAssignmentFunc env@(Env u) name args body = do
  let proc = ProcLisp args body env'
      env' = Env $ (name, ProcObj proc):u
  return $ env'
  

-- Evaluate application of a list of arguments to a function
apply :: Proc -> [LispObj] -> Ret LispObj
apply (ProcPrim prim) ps = applyPrim prim ps
-- Apply lisp procedure
apply (ProcLisp params body (Env env)) args
  = if length params == length args
      then let env' =  Env ((zip params args) ++ env)
           in eval env' body
      else Left $ LispError $ show "Argument number mismatch"

-- Evaluate 'primative' functions, built in procedures
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

-- Evaluate if else expression
evalIf :: Env -> Lisp -> Lisp -> Lisp -> Ret LispObj
evalIf env cond true false = do
  x <- eval env cond
  case x of
    PrimBool b -> eval env (if b then true else false)
    y          -> Left $ LispError $ "Expected bool, got: " ++ show y

-- Lookup a variable from the environment
lookupVar :: Env -> Variable -> Ret LispObj
lookupVar (Env u) v
  = maybe (Left (LispError $ "Variable not defined " ++ show v)) Right (lookup v u)

