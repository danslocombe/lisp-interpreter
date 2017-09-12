{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Text.Parsec
import Text.Parsec.Char
import Data.List (intersperse)
import Data.Either (isRight)
-- import Text.Parsec.Token

newtype Fix f = Fx (f (Fix f))
unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x

type Expr = Fix ExprF
type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> (Fix f -> a)
cata alg = alg . fmap (cata alg) . unFix

newtype Variable = Variable String deriving (Show, Eq)

-- instance Show Variable where
  -- show (Variable v) = v

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
  | PFMinus
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

primFuncSymbols :: [(String, PrimFunc)] 
primFuncSymbols =
  [ ("+", PFPlus)
  , ("-", PFMinus)
  , ("cons", PFCons)
  , ("car", PFCar)
  , ("cdr", PFCdr)
  ]

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

showExprF :: Algebra ExprF String
showExprF e = case e of
  Var v -> show v
  Obj p -> show p
  If _ _ _ -> "If expression"
  Def v [] val next -> show v ++ " = " ++ show val ++ " in " ++ next
  App x args -> "( " ++ show x ++ concat (intersperse " " (map show args)) ++ ")"
  _ -> "OTHER"

showExpr :: Expr -> String
showExpr = cata showExprF

instance Show Expr where
  show = showExpr

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

testSource :: String
testSource = "(define x 2)\n(+ x 2)"

parseInt :: Parsec String st Expr
parseInt = Fx . Obj . PrimInt . read <$> (many1 $ oneOf "0123456789")

parseVar :: Parsec String st Variable
parseVar = Variable <$> many1 letter

whitespace :: Parsec String st ()
whitespace
  =   spaces 
  <|> ((string "\r\n") >> return ())
  <|> ((string "\n") >> return ())

parseOverall :: Parsec String st Expr
parseOverall
  =   try (do {x <- parseBracket parseDefine; x <$> parseOverall})
  <|> try parseExpr

parseExpr :: Parsec String st Expr
parseExpr
  =   do
  spaces
  try (parseBracket parseApply) 
    <|> try (Fx . Var <$> parseVar)
    <|> try parseInt
    <|> try (Fx . Obj . ProcObj <$> parsePrimProc)

parseBracket :: Parsec String st a -> Parsec String st a
parseBracket p = do
  try whitespace
  string "("
  spaces
  x <- p
  spaces
  string ")"
  try whitespace
  return x

parseApply :: Parsec String st Expr
parseApply = do
  proc <- parseExpr
  args <- many1 parseExpr
  return $ Fx $ App proc args

parsePrimProc :: Parsec String st Proc
parsePrimProc = do
  string "+"
  return (ProcPrim PFPlus)

parsePrimFunc :: String -> Either ParseError PrimFunc
parsePrimFunc s = maybe err Right (lookup s primFuncSymbols)
  where err = undefined

parseDefine :: Parsec String st (Expr -> Expr)
parseDefine = do
  string "define"
  spaces
  parseDefineVar <|> parseDefineProc

parseDefineVar :: Parsec String st (Expr -> Expr)
parseDefineVar = do
  v <- parseVar
  spaces
  i <- parseInt
  return $ \next -> Fx $ Def v [] i next

parseDefineProc :: Parsec String st (Expr -> Expr)
parseDefineProc = do
  (name:params) <- parseBracket (many1 parseVar)
  spaces
  body <- parseExpr
  return $ \next -> Fx $ Def name params body next

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
