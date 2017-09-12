module Parse where

import Types

import Text.Parsec
import Text.Parsec.Char

-- Mapping of basic, primative functions to their respective AST elem
primFuncSymbols :: [(String, PrimFunc)] 
primFuncSymbols =
  [ ("+", PFPlus)
  , ("-", PFMinus)
  , ("<", PFLess)
  , (">", PFGreater)
  , ("=", PFEq)
  , ("cons", PFCons)
  , ("car", PFCar)
  , ("cdr", PFCdr)
  , ("list", PFList)
  , ("null?", PFNullCheck)
  ]

parseLisp :: Parsec String st Expr
parseLisp
  =   try (do {x <- parseBracket parseDefine; x <$> parseLisp})
  <|> try parseExpr

parseExpr :: Parsec String st Expr
parseExpr = do
  spaces
  try parseNil
  <|> try (parseBracket parseIf) 
  <|> try (parseBracket parseApply) 
  <|> try (Fx . Obj . ProcObj <$> parsePrimProc)
  <|> try parseInt
  <|> try (Fx . Var <$> parseVar)

parseNil :: Parsec String st Expr
parseNil = string "nil" >> (return $ Fx $ Obj $ Nil)

parseIf = do
  string "if"
  spaces
  predicate <- parseExpr
  spaces
  string "then"
  spaces
  thenExpr <- parseExpr
  spaces
  string "else"
  spaces
  elseExpr <- parseExpr
  return $ Fx $ If predicate thenExpr elseExpr
  
parseBracket :: Parsec String st a -> Parsec String st a
parseBracket p = do
  try whitespace
  string "("
  try spaces
  x <- p
  try spaces
  string ")"
  try whitespace
  return x

parseApply :: Parsec String st Expr
parseApply = do
  proc <- parseExpr
  spaces
  args <- sepEndBy parseExpr spaces
  return $ Fx $ App proc args

parsePrimProc :: Parsec String st Proc
parsePrimProc = 
  let tryParsePrim = map (\(symbol, prim) -> try ( do {
        string symbol; 
        return (ProcPrim prim)}))
          primFuncSymbols
  in foldl1 ((<|>)) tryParsePrim

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

parseInt :: Parsec String st Expr
parseInt = Fx . Obj . PrimInt . read <$> (many1 digit)

parseVar :: Parsec String st Variable
parseVar = Variable <$> many1 (try letter <|> try (oneOf "-?!"))

whitespace :: Parsec String st ()
whitespace
  =   spaces 
  <|> ((string "\r\n") >> return ())
  <|> ((string "\n") >> return ())
