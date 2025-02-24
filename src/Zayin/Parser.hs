{-# LANGUAGE OverloadedStrings #-}
module Zayin.Parser (parseProgram) where

import Control.Monad.Logger (logDebugN, runStderrLoggingT, LoggingT)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace, isDigit, isAlpha, isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T

import Zayin.AST
import Zayin.Literals (Literal(..))

--------------------------------------------------------------------------------
-- Token Definitions and Lexer with Layout Insertion
--------------------------------------------------------------------------------

data Token
  = TIdent Text
  | TNumber Integer
  | TKeyword Text
  | TSymbol Text
  | TString Text
  | TNewline
  | TIndent
  | TDedent
  | TEOF
  deriving (Show, Eq)

lexZayin :: Text -> IO [Token]
lexZayin input = runStderrLoggingT $ do
  logDebugN "Lexing input with layout processing"
  let rawLines = T.lines input
  toks <- lexLines rawLines [] 0
  return (toks ++ [TEOF])
  where
    lexLines :: [Text] -> [Int] -> Int -> LoggingT IO [Token]
    lexLines [] indentStack _ = return (dedentAll indentStack)
    lexLines (l:ls) indentStack _ = do
      let currentIndent = T.length (T.takeWhile isSpace l)
          lineContent   = T.strip l
      logDebugN $ "Lexing line: " <> l <> " (indent " <> T.pack (show currentIndent) <> ")"
      let (indentToks, newStack) = computeIndentTokens currentIndent indentStack
      toksLine <- lexLineTokens lineContent
      restToks <- lexLines ls newStack currentIndent
      return (indentToks ++ toksLine ++ [TNewline] ++ restToks)

    dedentAll :: [Int] -> [Token]
    dedentAll []     = []
    dedentAll (_:xs) = TDedent : dedentAll xs

    computeIndentTokens :: Int -> [Int] -> ([Token], [Int])
    computeIndentTokens cur [] = ([TIndent], [cur])
    computeIndentTokens cur stack@(s:rest)
      | cur > s   = ([TIndent], cur : stack)
      | cur == s  = ([], stack)
      | cur < s   = let (deds, newStack) = popUntil cur stack
                    in (deds, newStack)
    popUntil :: Int -> [Int] -> ([Token], [Int])
    popUntil cur [] = ([], [])
    popUntil cur (s:ss)
      | cur == s  = ([], s:ss)
      | cur < s   = let (d, ns) = popUntil cur ss in (TDedent : d, ns)
      | otherwise = ([], s:ss)

    lexLineTokens :: Text -> LoggingT IO [Token]
    lexLineTokens txt = do
      let toks = tokenizeLine txt
      logDebugN $ "Tokenizing line: " <> txt <> " -> " <> T.pack (show toks)
      return toks

tokenizeLine :: Text -> [Token]
tokenizeLine t =
  let t' = T.stripStart t
  in if T.null t' then [] else nextToken t'

nextToken :: Text -> [Token]
nextToken t
  | T.null t = []
  | otherwise =
      let t' = T.stripStart t
      in if T.null t' then [] else
         case T.uncons t' of
           Nothing -> []
           Just (c, rest)
             | c == '"' ->  -- Put string handling first
                 case T.breakOn "\"" rest of
                   (str, rest')
                     | T.null rest' -> error "Unterminated string literal"
                     | otherwise ->
                         let remaining = T.drop 1 rest'  -- Skip the closing quote
                         in TString str : nextToken remaining
             | isDigit c ->
                 let (num, rest') = T.span isDigit t'
                     token = TNumber (read (T.unpack num))
                 in token : nextToken rest'
             | isAlpha c ->
                 let (word, rest') = T.span isAlphaNum t'
                     token = if isKeyword word then TKeyword word else TIdent word
                 in token : nextToken rest'
             | isPunctuation c ->
                 TSymbol (T.singleton c) : nextToken rest
             | otherwise -> nextToken rest

isPunctuation :: Char -> Bool
isPunctuation c = c `elem` ("()+-*/=,:;" :: String)

isKeyword :: Text -> Bool
isKeyword w = w `elem` ["if", "then", "else", "fn", "where", "do", "true", "false", "nil"]

isBuiltin :: Text -> Bool
isBuiltin w = w `elem` ["display"]

--------------------------------------------------------------------------------
-- Parser Combinators using LoggingT IO (with layout skipping)
--------------------------------------------------------------------------------

newtype Parser a = Parser { runParser :: [Token] -> LoggingT IO (Either String (a, [Token])) }

instance Functor Parser where
  fmap f p = Parser $ \ts -> do
    res <- runParser p ts
    return $ fmap (\(a, ts') -> (f a, ts')) res

instance Applicative Parser where
  pure a = Parser $ \ts -> return (Right (a, ts))
  pf <*> pa = Parser $ \ts -> do
    resF <- runParser pf ts
    case resF of
      Left err -> return (Left err)
      Right (f, ts') -> do
         resA <- runParser pa ts'
         case resA of
           Left err -> return (Left err)
           Right (a, ts'') -> return (Right (f a, ts''))

instance Monad Parser where
  return = pure
  p >>= f = Parser $ \ts -> do
    res <- runParser p ts
    case res of
      Left err -> return (Left err)
      Right (a, ts') -> runParser (f a) ts'

infixr 3 <|>
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser $ \ts -> do
  res <- runParser p ts
  case res of
    Right _ -> return res
    Left _  -> runParser q ts

-- Consume a token (skipping layout tokens).
consume :: (Token -> Bool) -> Parser Token
consume pred = Parser $ \ts ->
  let ts' = dropWhile isLayout ts
  in case ts' of
       (t:rest) | pred t -> do
           logDebugN $ "Consumed token: " <> T.pack (show t)
           return (Right (t, rest))
       _ -> return (Left "Unexpected token")
  where
    isLayout t = case t of
      TIndent  -> True
      TDedent  -> True
      TNewline -> True
      _        -> False

-- Parse many occurrences.
many :: Parser a -> Parser [a]
many p = (do
  x <- p
  xs <- many p
  return (x:xs)) <|> return []

--------------------------------------------------------------------------------
-- Top-Level Statement Parsers Producing ExprBodyExpr
--------------------------------------------------------------------------------

-- A top-level statement is either a function definition or a plain expression.
parseTopStmt :: Parser ExprBodyExpr
parseTopStmt = parseFnDef <|> (do
  e <- parseExpr
  return (Expr e))

-- | Parse a function definition of the form:
--    fn <name>(<args>): <body>
parseFnDef :: Parser ExprBodyExpr
parseFnDef = do
  _ <- consume (== TKeyword "fn")
  nameToken <- consume (\t -> case t of TIdent _ -> True; _ -> False)
  let name = case nameToken of TIdent n -> n; _ -> ""
  _ <- consume (== TSymbol "(")
  args <- parseArgs
  _ <- consume (== TSymbol ")")
  _ <- consume (== TSymbol ":")
  bodyExpr <- parseExpr
  -- Wrap the function body in a lambda with no definitions.
  return (Def name (ELam args (ExprBody [] bodyExpr)))

-- | Parse a comma-separated list of arguments (identifiers).
parseArgs :: Parser [Text]
parseArgs = (do
  argToken <- consume (\t -> case t of TIdent _ -> True; _ -> False)
  let arg = case argToken of TIdent a -> a; _ -> ""
  rest <- many (do
    _ <- consume (== TSymbol ",")
    tok <- consume (\t -> case t of TIdent _ -> True; _ -> False)
    return (case tok of TIdent a -> a; _ -> ""))
  return (arg:rest)) <|> return []

-- | Parse a sequence of top-level statements.
parseProgramExprs :: Parser [ExprBodyExpr]
parseProgramExprs = many parseTopStmt

--------------------------------------------------------------------------------
-- Expression Parsers Producing Our Actual AST
--------------------------------------------------------------------------------

-- Expressions include if-expressions, addition, function calls, and primaries.
parseExpr :: Parser Expr
parseExpr = parseIf <|> parseAdd

parseExprList :: Parser Expr
parseExprList = do
  exprs <- sepBy parseExpr (consume (== TSymbol ";"))
  return $ case exprs of
    [] -> ELit LNil
    [x] -> x
    xs -> foldr1 makeSequence xs
  where
    makeSequence e1 e2 =
      -- Create a sequence by applying each expression to display
      EApp (ELam ["_"] (ExprBody [] e2)) [e1]

-- Parse a list of expressions separated by semicolons
parseIf :: Parser Expr
parseIf = do
  _    <- consume (== TKeyword "if")
  cond <- parseExpr
  _    <- consume (== TSymbol ":")
  thenE <- parseExprList
  return (EIf cond thenE (ELit LNil))

-- Helper for parsing separated lists
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = do
  first <- p
  rest <- many (do
    _ <- sep
    p)
  return (first : rest)

-- Parse left-associative addition.
parseAdd :: Parser Expr
parseAdd = do
  lhs <- parseCall
  moreAdd lhs
  where
    moreAdd lhs = (do
      _ <- consume (== TSymbol "+")
      rhs <- parseCall
      let addExpr = EApp (EBuiltinIdent "+") [lhs, rhs]
      moreAdd addExpr) <|> return lhs

-- Parse function application with either space or parentheses syntax
parseCall :: Parser Expr
parseCall = do
  primary <- parsePrimary
  parseCall' primary
  where
    parseCall' fun =
      -- Try parentheses syntax first
      (do
        _ <- consume (== TSymbol "(")
        arg <- parseExpr
        _ <- consume (== TSymbol ")")
        let callExpr = EApp fun [arg]
        parseCall' callExpr)
      <|>
      -- Try space-separated syntax for builtins
      (case fun of
        EBuiltinIdent _ -> do
          arg <- parsePrimary  -- Use parsePrimary to avoid left recursion
          let callExpr = EApp fun [arg]
          parseCall' callExpr
        _ -> return fun)
      <|> return fun

-- Parse a primary expression.
parsePrimary :: Parser Expr
parsePrimary = (do
  _ <- consume (== TSymbol "(")
  e <- parseExpr
  _ <- consume (== TSymbol ")")
  return e) <|> parseTermSimple

-- Parse a simple term: identifier or number.
parseTermSimple :: Parser Expr
parseTermSimple = do
  t <- consume isTerm
  case t of
    TIdent name -> if (isBuiltin name)
                   then return (EBuiltinIdent name)
                   else return (EVar name)
    TNumber n -> return (ELit (LInt n))
    TString s -> return (ELit (LString s))  -- Handle string literals
    TKeyword "nil" -> return (ELit LNil)
    TKeyword "true" -> return (ELit (LBool True))
    TKeyword "false" -> return (ELit (LBool False))
    _ -> Parser $ \_ -> return (Left "Expected a term")
  where
    isTerm tok = case tok of
      TIdent _  -> True
      TNumber _ -> True
      TString _ -> True
      TKeyword k -> k `elem` ["nil", "true", "false"]
      _         -> False

--------------------------------------------------------------------------------
-- Helpers to Combine Top-Level Statements
--------------------------------------------------------------------------------

-- Partition a list of top-level statements into definitions and expressions.
partitionDefs :: [ExprBodyExpr] -> ([ExprBodyExpr], [Expr])
partitionDefs = foldr f ([], [])
  where
    f (Def n e) (defs, exprs) = (Def n e : defs, exprs)
    f (Expr e) (defs, exprs)  = (defs, e : exprs)

-- Combine a list of top-level statements into a single AST.
combineStmts :: [ExprBodyExpr] -> Expr
combineStmts stmts =
  let (defs, exprs) = partitionDefs stmts
  in case exprs of
       []  -> error "No final expression in program"
       [e] -> EApp (ELam [] (ExprBody defs e)) []
       _   -> let finalExpr = last exprs
                  preceding   = init exprs
              in EApp (ELam [] (ExprBody (defs ++ map Expr preceding) finalExpr)) []

--------------------------------------------------------------------------------
-- Top-Level parseProgram Function
--------------------------------------------------------------------------------

dropLayout :: [Token] -> [Token]
dropLayout = dropWhile (\t -> case t of
                                TIndent  -> True
                                TDedent  -> True
                                TNewline -> True
                                _        -> False)

parseProgram :: Text -> IO (Either String Expr)
parseProgram input = do
  toks <- lexZayin input
  runStderrLoggingT $ do
    logDebugN $ "Tokens after layout: " <> T.pack (show toks)
    res <- runParser parseProgramExprs toks
    case res of
      Right (stmts, remaining) ->
         if dropLayout remaining == [TEOF]
            then do
              let progAst = combineStmts stmts
              logDebugN $ "Parsing complete: " <> T.pack (show progAst)
              return (Right progAst)
            else do
              logDebugN $ "Unconsumed tokens: " <> T.pack (show remaining)
              return (Left ("Parsing error: unconsumed tokens " ++ show remaining))
      Left err -> return (Left err)
