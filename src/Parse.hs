module Parse where

import Types

import Text.Parsec
import Text.Parsec.Char

primFuncSymbols :: [(String, PrimFunc)] 
primFuncSymbols =
  [ ("+", PFPlus)
  , ("-", PFMinus)
  , ("cons", PFCons)
  , ("car", PFCar)
  , ("cdr", PFCdr)
  ]


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
  let p = do {spaces; x<-parseVar; spaces; return x}
  (name:params) <- parseBracket (many p)
  spaces
  body <- parseExpr
  return $ \next -> Fx $ Def name params body next
