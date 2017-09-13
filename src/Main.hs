{-# LANGUAGE DeriveFunctor #-}

module Main where

import Types
import Parse
import Eval

import Text.Parsec
import Text.Parsec.Char
import Data.Either (isRight)
import Control.Monad (forever, foldM)

emptyEnv :: Env
emptyEnv = Env []

concatEnvs :: Env -> Env -> Env
concatEnvs (Env e) (Env e') = Env $ e ++ e'

-- Eval for repl
lispRepl :: Env -> String -> Ret (ReplEnv (Maybe LispObj))
lispRepl env s = 
  let x = runParser parseLispRepl "" "" s
  in case x of
    Left err -> Left $ LispError $ show err
    Right (LRDef def) -> do
                 { d <- evalDefinition env def
                 ; return $ ReplEnv d Nothing }
    Right (LRObj lisp) -> do
                  { x <- eval env lisp
                  ; return $ ReplEnv env (Just x) }

-- Run repl forever
repl :: Env -> String -> IO a
repl env s = do 
  if allWhitespace s
  -- If whitespace entered, ignore it and read another line
  then do
    s' <- getLine
    repl env s'
  -- Otherwise try and parse it
  else do
    env' <- case lispRepl env s of
      Left err -> putStrLn (show err) >> return env
      -- Parsed a definition
      Right (ReplEnv e Nothing) -> putStrLn "Ok" >> return e
      -- Parsed an expression
      Right (ReplEnv e2 (Just x)) -> putStrLn (show x) >> return e2
    s' <- getLine
    repl env' s'

-- Run over source file
lispIO :: String -> IO ()
lispIO s =
  let x = runParser parseLisp "" "" s
  in putStrLn $ case x of
    Left err -> "Parsing error: " ++ show err
    Right parse -> case eval emptyEnv parse of
      Left err -> "Runtime error: " ++ show err
      Right ran -> show ran

main :: IO ()
main = do
  putStrLn "Loading Prelude.."
  env <- loadPrelude
  repl env ""

allWhitespace :: String -> Bool
allWhitespace s = all (\x -> (elem x " \r\n\t")) s

loadPrelude :: IO Env
loadPrelude = fromRight $ foldM interpretExtractEnv emptyEnv prelude
  where 
    interpretExtractEnv env source = extractEnv <$> (lispRepl env source)
    extractEnv (ReplEnv env _) = env
    fromRight x = case x of
      Right y -> putStrLn ("Loaded " ++ (show $ length prelude) ++ " functions")  >> return y
      Left z -> (putStrLn $ show z) >> return emptyEnv

-- -- -- -- -- -- -- -- -- -- -- -- -- --
--           Lisp programs             --
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
