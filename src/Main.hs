{-# LANGUAGE DeriveFunctor #-}

module Main where

import Types
import Parse

import Text.Parsec
import Text.Parsec.Char
import Data.Either (isRight)
import Control.Monad (forever, foldM)

newtype LispError = LispError String 
  deriving Show

data ReplEnv a = ReplEnv Env a deriving (Show, Functor)

emptyEnv :: Env
emptyEnv = Env []

concatEnvs :: Env -> Env -> Env
concatEnvs (Env e) (Env e') = Env $ e ++ e'

type Ret a = Either LispError a

lispRepl :: Env -> String -> Ret (ReplEnv (Maybe LispObj))
lispRepl env s = 
  let x = runParser parseLispRepl "" "" s
  in case x of
    Left err -> Left $ LispError $ show err
    Right parse -> case parse of
      LRDef def -> do
                   { d <- evalDefinition env def
                   ; return $ ReplEnv d Nothing }
      LRObj lisp -> do
                    { x <- eval env lisp
                    ; return $ ReplEnv env (Just x) }

lispIO :: String -> IO ()
lispIO s =
  let x = runParser parseLisp "" "" s
  in putStrLn $ case x of
    Left err -> "Parsing error: " ++ show err
    Right parse -> case eval emptyEnv parse of
      Left err -> "Runtime error: " ++ show err
      Right ran -> show ran
    
-- main :: IO ()
-- main = forever (getLine >>= lispIO) 

allWhitespace :: String -> Bool
allWhitespace s = all (\x -> (elem x " \r\n\t")) s

loadPrelude :: IO Env
loadPrelude = fromRight $ foldM f emptyEnv prelude
  where 
    f env source = extractEnv <$> (lispRepl env source)
    extractEnv (ReplEnv env _) = env
    fromRight x = case x of
      Right y -> putStrLn ("Loaded " ++ (show $ length prelude) ++ " functions")  >> return y
      Left z -> (putStrLn $ show z) >> return emptyEnv

main :: IO ()
-- main = f emptyEnv prelude
main = do
  putStrLn "Loading Prelude.."
  env <- loadPrelude
  f env ""
  where
    f env s = do 
      if allWhitespace s
      then do
        s' <- getLine
        f env s'
      else do
        env' <- case lispRepl env s of
          Left err -> putStrLn (show err) >> return env
          Right (ReplEnv e Nothing) -> putStrLn "Ok" >> return e
          Right (ReplEnv e2 (Just x)) -> putStrLn (show x) >> return e2
        s'' <- getLine
        f env' s''

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
  -- TODO make sure size of args = size ps
apply (ProcLisp args body (Env env)) ps
  = let env' =  Env ((zip args ps) ++ env)
    in eval env' body

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

-- -- -- -- -- -- -- -- -- -- -- -- -- --
--        Test lisp programs           --
-- -- -- -- -- -- -- -- -- -- -- -- -- --

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
    ")"                             ++ "\n"

lispSourceFoldL :: String
lispSourceFoldL 
  = "(define (foldl f x xs)"               ++ "\n" ++
      "(if (null? xs)"                     ++ "\n" ++
         "then x"                          ++ "\n" ++
         "else ("                          ++ "\n" ++
           "foldl f (f x (car xs)) (cdr xs)" ++ "\n" ++
        ")"                                ++ "\n" ++
      ")"                                  ++ "\n" ++
    ")"                                    ++ "\n"

lispSourceId :: String
lispSourceId = "(define (id x) x)"

lispSourceAdd :: String
lispSourceAdd = "(define (add x y) (+ x y))"

lispSourceSum :: String
lispSourceSum = "(define (sum xs) (foldl add 0 xs))"

prelude = 
  [ lispSourceMap
  , lispSourceFoldL
  , lispSourceId
  , lispSourceAdd
  , lispSourceSum
  ]

lispSourceMapUse = lispSourceMap    ++ "\n" ++
  " (define (double x) (+ x x)) "   ++ "\n" ++
  " (map double (list 1 2 3 4 5))"
