{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Zayin.Parser (parseProgram) where

import Control.Monad.Logger (MonadLogger(..), LoggingT, logDebugN, runStderrLoggingT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Char (isSpace, isDigit, isAlpha, isAlphaNum)
import qualified Data.Text as T
import Data.Text (Text)
import Zayin.AST
import Zayin.Literals

-- List of built-in function names.
builtinNames :: [T.Text]
builtinNames =
  [ "tostring", "display", "exit", "+", "-", "*", "/", "%", "^"
  , "<", "<=", ">", ">=", "not", "cons", "cons?", "null?", "car"
  , "cdr", "string-concat", "string-chars", "ht-new", "ht-set!"
  , "ht-get", "ht-del!", "ht-keys", "eq?" ]

isBuiltin :: T.Text -> Bool
isBuiltin ident = ident `elem` builtinNames

-- Each input line annotated with its indentation level.
data Line = Line { indent :: Int, content :: T.Text }
  deriving (Show)

tokenizeLines :: T.Text -> [Line]
tokenizeLines txt =
  let ls = T.lines txt
  in map (\l -> let i = T.length (T.takeWhile isSpace l)
                    c = T.stripStart l
                in Line i c) ls

-- Our parser type now runs in a LoggingT IO monad.
newtype Parser a = Parser { runParser :: [Line] -> LoggingT IO (Either String (a, [Line])) }

instance Functor Parser where
  fmap f p = Parser $ \s -> do
    res <- runParser p s
    return (fmap (\(a, s') -> (f a, s')) res)

instance Applicative Parser where
  pure a = Parser $ \s -> return (Right (a, s))
  pf <*> pa = Parser $ \s -> do
    r1 <- runParser pf s
    case r1 of
      Left err       -> return (Left err)
      Right (f, s')  -> do
        r2 <- runParser pa s'
        case r2 of
          Left err       -> return (Left err)
          Right (a, s'') -> return (Right (f a, s''))

instance Monad Parser where
  return = pure
  p >>= f = Parser $ \s -> do
    r1 <- runParser p s
    case r1 of
      Left err      -> return (Left err)
      Right (a, s') -> runParser (f a) s'

instance MonadIO Parser where
  liftIO io = Parser $ \s -> do
    a <- liftIO io
    return (Right (a, s))

-- Manually define a MonadLogger instance so we can call logDebugN, etc.
instance MonadLogger Parser where
  monadLoggerLog loc src lvl msg = Parser $ \s -> do
    monadLoggerLog loc src lvl msg
    return (Right ((), s))

-- | Log a message and then fail.
failParser :: String -> Parser a
failParser msg = Parser $ \_ -> do
  logDebugN ("failParser: " <> T.pack msg)
  return (Left msg)

-- | Consume a line if it satisfies the predicate.
satisfyLine :: (Line -> Bool) -> Parser Line
satisfyLine p = Parser $ \s -> case s of
  [] -> do
    logDebugN "satisfyLine: Unexpected end of input"
    return (Left "Unexpected end of input")
  (l:ls) ->
    if p l
      then do
        logDebugN ("satisfyLine: Consuming line: " <> content l)
        return (Right (l, ls))
      else do
        logDebugN ("satisfyLine: Unexpected line: " <> content l)
        return (Left ("Unexpected line: " ++ T.unpack (content l)))

-- | Peek at the next line without consuming it.
peekLine :: Parser (Maybe Line)
peekLine = Parser $ \s -> do
  logDebugN "peekLine: Peeking at next line"
  return (Right (case s of [] -> Nothing; (l:_) -> Just l, s))

-- | Consume the next line and return its content.
consumeLine :: Parser T.Text
consumeLine = do
  l <- satisfyLine (const True)
  logDebugN ("consumeLine: " <> content l)
  return (content l)

-- | Parse an identifier from text.
parseIdentifier :: T.Text -> (T.Text, T.Text)
parseIdentifier txt =
  let (ident, rest) = T.span isAlphaNum txt
  in (ident, rest)

-- | Parse a number literal.
parseNumber :: T.Text -> (Literal, T.Text)
parseNumber txt =
  let (numTxt, rest) = T.span isDigit txt
      num = read (T.unpack numTxt) :: Integer
  in (LInt num, rest)

-- | Parse a “term” from a single line.
parseTerm :: Parser Expr
parseTerm = do
  logDebugN "parseTerm: Start"
  txt <- consumeLine
  let txt' = T.strip txt
  logDebugN ("parseTerm: Processing text: " <> txt')
  if T.null txt'
    then do
      logDebugN "parseTerm: Empty term encountered"
      failParser "Empty term"
    else case T.head txt' of
      '(' -> do
         logDebugN ("parseTerm: Found parenthesized expression: " <> txt')
         case T.unsnoc (T.tail txt') of
           Just (inside, ')') -> do
              logDebugN ("parseTerm: Inside parenthesis: " <> inside)
              return (parseExprText inside)
           _ -> failParser "Unmatched parenthesis"
      c | isDigit c -> do
            logDebugN ("parseTerm: Found digit literal: " <> txt')
            let (lit, _) = parseNumber txt'
            return (ELit lit)
        | isAlpha c -> do
            logDebugN ("parseTerm: Found identifier: " <> txt')
            let (ident, rest) = parseIdentifier txt'
            if isBuiltin ident
                 then if T.null rest
                        then do
                          logDebugN ("parseTerm: Built-in identifier (no args): " <> ident)
                          return (EBuiltinIdent ident)
                        else if T.head rest == '('
                               then do
                                  let argsTxt = T.init (T.tail rest)
                                  logDebugN ("parseTerm: Built-in function application: " <> ident <> " with args: " <> argsTxt)
                                  let argExpr = parseExprText argsTxt
                                  return (EApp (EBuiltinIdent ident) [argExpr])
                               else do
                                  logDebugN ("parseTerm: Built-in identifier with trailing text: " <> rest)
                                  return (EBuiltinIdent ident)
                 else if T.null rest
                        then do
                          logDebugN ("parseTerm: Variable: " <> ident)
                          return (EVar ident)
                        else if T.head rest == '('
                                then do
                                  let argsTxt = T.init (T.tail rest)
                                  logDebugN ("parseTerm: Variable function application: " <> ident <> " with args: " <> argsTxt)
                                  return (EApp (EVar ident) [parseExprText argsTxt])
                                else do
                                  logDebugN ("parseTerm: Variable with trailing text: " <> rest)
                                  return (EVar ident)
      _ -> do
            logDebugN ("parseTerm: Cannot parse term: " <> txt')
            failParser ("Cannot parse term: " ++ T.unpack txt')

-- | A helper to parse an expression from text.
parseExprText :: T.Text -> Expr
parseExprText txt =
  if T.any isSpace txt
     then let parts = T.words txt
              left = if T.all isDigit (head parts)
                        then ELit (LInt (read (T.unpack (head parts)) :: Integer))
                        else EVar (head parts)
              right = case parts of
                        (_:_:n:_) -> ELit (LInt (read (T.unpack n) :: Integer))
                        _         -> ELit (LInt 0)
          in EApp (EBuiltinIdent "+") [left, right]
     else EVar txt

-- | Parse a statement within a block.
parseStmt :: Int -> Parser ExprBodyExpr
parseStmt baseIndent = do
  logDebugN ("parseStmt: baseIndent = " <> T.pack (show baseIndent))
  ml <- peekLine
  case ml of
    Nothing -> failParser "Unexpected end of input in block"
    Just l ->
      if indent l < baseIndent
         then do
           logDebugN ("parseStmt: Dedent encountered at line: " <> content l)
           failParser "Dedent encountered"
         else if "fn " `T.isPrefixOf` content l
                 then parseFnDef
                 else do
                   logDebugN ("parseStmt: Parsing expression at line: " <> content l)
                   expr <- parseExpr
                   return (Expr expr)

-- | Parse an expression (currently just a term).
parseExpr :: Parser Expr
parseExpr = parseTerm

-- | Parse a block (one or more statements at a given indent).
parseBlock :: Int -> Parser Expr
parseBlock baseIndent = do
  logDebugN ("parseBlock: baseIndent = " <> T.pack (show baseIndent))
  stmts <- many (parseStmt baseIndent)
  if null stmts
     then do
       logDebugN "parseBlock: Empty block encountered"
       failParser "Empty block"
     else do
       logDebugN ("parseBlock: Combining " <> T.pack (show (length stmts)) <> " statements")
       return (combineStmts stmts)

-- | Parse a function definition, yielding a Def node.
parseFnDef :: Parser ExprBodyExpr
parseFnDef = do
  logDebugN "parseFnDef: Parsing function definition"
  line <- satisfyLine (\l -> "fn " `T.isPrefixOf` content l)
  let rest    = T.strip $ T.drop 3 (content line)
      (fname, rest1) = parseIdentifier rest
      rest2   = T.strip rest1
  logDebugN ("parseFnDef: Function name parsed: " <> fname)
  if T.null rest2 || T.head rest2 /= '('
     then failParser "Expected '(' after function name"
     else do
       let rest3 = T.tail rest2  -- drop '('
           (paramsTxt, rest4) = T.breakOn ")" rest3
           params = if T.null paramsTxt
                      then []
                      else map T.strip (T.splitOn "," paramsTxt)
       logDebugN ("parseFnDef: Parameters: " <> T.intercalate ", " params)
       let afterParams = T.strip (T.drop 1 rest4)  -- drop ')'
       if not (":" `T.isPrefixOf` afterParams)
         then failParser "Expected ':' after function parameters"
         else do
           ml2 <- peekLine
           case ml2 of
             Nothing -> failParser "Expected block after function definition"
             Just l2 ->
               if indent l2 <= indent line
                  then failParser "Expected indented block after function definition"
                  else do
                    logDebugN "parseFnDef: Parsing function body"
                    body <- parseBlock (indent l2)
                    let exprBody = ExprBody [] body
                        lam      = ELam params exprBody
                    logDebugN ("parseFnDef: Function definition complete for " <> fname)
                    return (Def fname lam)

-- | Helper: parse zero or more occurrences.
many :: Parser a -> Parser [a]
many p = Parser $ \s -> do
  res <- runParser p s
  case res of
    Right (a, s') -> do
      resRest <- runParser (many p) s'
      case resRest of
        Right (as, s'') -> return (Right (a : as, s''))
        Left _          -> return (Right ([a], s'))
    Left _ -> return (Right ([], s))

-- | Partition a list of ExprBodyExpr into definitions and non-definition expressions.
partitionDefs :: [ExprBodyExpr] -> ([ExprBodyExpr], [Expr])
partitionDefs = foldr f ([], [])
  where
    f e (defs, exprs) = case e of
      Def name expr -> (Def name expr : defs, exprs)
      Expr expr   -> (defs, expr : exprs)

-- | Combine a list of ExprBodyExpr into a single Expr.
combineStmts :: [ExprBodyExpr] -> Expr
combineStmts stmts =
  let (defs, exprs) = partitionDefs stmts
  in case exprs of
       []  -> error "No final expression in block"
       [e] -> EApp (ELam [] (ExprBody defs e)) []
       _   -> let finalExpr = last exprs
                  preceding   = init exprs
              in EApp (ELam [] (ExprBody (defs ++ map Expr preceding) finalExpr)) []

-- | Top-level: parse an entire program.
parseProgram :: T.Text -> IO (Either String Expr)
parseProgram txt = runStderrLoggingT $ do
  let ls = tokenizeLines txt
  logDebugN "parseProgram: Starting parsing of program"
  result <- runParser (many (parseStmt 0)) ls
  case result of
       Right (stmts, []) -> do
         logDebugN "parseProgram: Parsing complete"
         return (Right (combineStmts stmts))
       Right (_, remaining) -> do
         logDebugN ("parseProgram: Unconsumed input: " <> T.pack (show remaining))
         return (Left ("Unconsumed input: " ++ show remaining))
       Left err -> do
         logDebugN ("parseProgram: Error: " <> T.pack err)
         return (Left err)
