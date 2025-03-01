{-# LANGUAGE OverloadedStrings #-}

module Zayin.Parser (parseProgram, parseWithDebug) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logDebugN, logErrorN, logInfoN, runStderrLoggingT)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Zayin.AST
import Zayin.Literals (Literal (..))

--------------------------------------------------------------------------------
-- Debug Context
--------------------------------------------------------------------------------

-- Add a reader monad to track debug state
type ParserM a = ReaderT Bool (LoggingT IO) a

-- Debug log helper that only logs when debug mode is enabled
debugLog :: Text -> ParserM ()
debugLog msg = do
  debugEnabled <- ask
  when debugEnabled $ logDebugN msg

-- Enhanced logger that always prints - for critical debugging
forceDebugLog :: String -> ParserM ()
forceDebugLog msg = do
  logInfoN $ T.pack $ "PARSER-DEBUG: " ++ msg

-- Debug logging that works within Parser monad
logInParser :: String -> Parser ()
logInParser msg = Parser $ \ts -> do
  forceDebugLog msg
  return (Right ((), ts))

-- Helper to inspect the token stream without consuming
inspectTokenStream :: Int -> Parser [Token]
inspectTokenStream n = Parser $ \ts -> do
  let visibleTokens = take n ts
  forceDebugLog $ "TOKENS: " ++ show visibleTokens
  return (Right (visibleTokens, ts))

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

lexZayin :: Bool -> Text -> IO [Token]
lexZayin debugMode input = runStderrLoggingT $ runReaderT lexAction debugMode
  where
    lexAction = do
      debugLog "Lexing input with layout processing"
      let rawLines = T.lines input
      toks <- lexLines rawLines [] 0
      return (toks ++ [TEOF])

    lexLines :: [Text] -> [Int] -> Int -> ParserM [Token]
    lexLines [] indentStack _ = return (dedentAll indentStack)
    lexLines (l : ls) indentStack _ = do
      let currentIndent = T.length (T.takeWhile isSpace l)
          lineContent = T.strip l
      debugLog $ "Lexing line: " <> l <> " (indent " <> T.pack (show currentIndent) <> ")"
      let (indentToks, newStack) = computeIndentTokens currentIndent indentStack
      toksLine <- lexLineTokens lineContent
      restToks <- lexLines ls newStack currentIndent
      return (indentToks ++ toksLine ++ [TNewline] ++ restToks)

    dedentAll :: [Int] -> [Token]
    dedentAll [] = []
    dedentAll (_ : xs) = TDedent : dedentAll xs

    computeIndentTokens :: Int -> [Int] -> ([Token], [Int])
    computeIndentTokens cur [] = ([TIndent], [cur])
    computeIndentTokens cur stack@(s : rest)
      | cur > s = ([TIndent], cur : stack)
      | cur == s = ([], stack)
      | cur < s =
          let (deds, newStack) = popUntil cur stack
           in (deds, newStack)

    popUntil :: Int -> [Int] -> ([Token], [Int])
    popUntil cur [] = ([], [])
    popUntil cur (s : ss)
      | cur == s = ([], s : ss)
      | cur < s = let (d, ns) = popUntil cur ss in (TDedent : d, ns)
      | otherwise = ([], s : ss)

    lexLineTokens :: Text -> ParserM [Token]
    lexLineTokens txt = do
      let toks = tokenizeLine txt
      debugLog $ "Tokenizing line: " <> txt <> " -> " <> T.pack (show toks)
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
       in if T.null t'
            then []
            else case T.uncons t' of
              Nothing -> []
              Just (c, rest)
                | c == '"' -> -- Put string handling first
                    case T.breakOn (T.pack "\"") rest of
                      (str, rest')
                        | T.null rest' -> error "Unterminated string literal"
                        | otherwise ->
                            let remaining = T.drop 1 rest' -- Skip the closing quote
                             in TString str : nextToken remaining
                | isDigit c ->
                    let (num, rest') = T.span isDigit t'
                        token = TNumber (read (T.unpack num))
                     in token : nextToken rest'
                | isAlpha c ->
                    let (word, rest') = T.span isIdentChar t'
                        token = if isKeyword word then TKeyword word else TIdent word
                     in token : nextToken rest'
                | isPunctuation c ->
                    -- Check for multi-character operators like <=, >=, ==
                    if c `elem` ['<', '>', '='] && not (T.null rest) && T.head rest == '=' then
                      TSymbol (T.pack [c, '=']) : nextToken (T.drop 1 rest)
                    else
                      TSymbol (T.singleton c) : nextToken rest
                | otherwise -> nextToken rest

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

isPunctuation :: Char -> Bool
isPunctuation c = c `elem` ("()+-*/=,:;<>!" :: String)

isKeyword :: Text -> Bool
isKeyword w = w `elem` [T.pack "if", T.pack "then", T.pack "else", T.pack "fn",
                       T.pack "where", T.pack "do", T.pack "true", T.pack "false",
                       T.pack "nil", T.pack "not"]

isBuiltin :: Text -> Bool
isBuiltin w = w `elem` [T.pack "display", T.pack "not", T.pack "gc_collect", T.pack "gc_stats", T.pack "spawn", T.pack "cons",
                        T.pack "+", T.pack "-", T.pack "*", T.pack "/", T.pack "%", T.pack "<", T.pack ">",
                        T.pack "<=", T.pack ">=", T.pack "eq?", T.pack "car", T.pack "cdr"]

--------------------------------------------------------------------------------
-- Parser Combinators using LoggingT IO (with layout skipping)
--------------------------------------------------------------------------------

newtype Parser a = Parser {runParser :: [Token] -> ParserM (Either String (a, [Token]))}

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
    Left _ -> runParser q ts

-- Enhanced consume with more detailed logging
consume :: (Token -> Bool) -> Parser Token
consume pred = Parser $ \ts -> do
  let ts' = dropWhile isLayout ts
  case ts' of
    (t : rest) | pred t -> do
      debugLog $ "Consumed token: " <> T.pack (show t)
      return (Right (t, rest))
    (t : _) -> do
      -- Log the context when consumption fails
      forceDebugLog $ "FAILED to consume token: " ++ show t
      forceDebugLog $ "First 5 tokens in stream: " ++ show (take 5 ts')
      return (Left $ "Unexpected token: " ++ show t)
    [] -> do
      debugLog "Failed to consume token, end of input"
      return (Left "Unexpected end of input")
  where
    isLayout t = case t of
      TIndent -> True
      TDedent -> True
      TNewline -> True
      _ -> False

-- Enhanced tryConsume with more logging
tryConsume :: (Token -> Bool) -> Parser (Maybe Token)
tryConsume pred = Parser $ \ts -> do
    -- Don't skip layout tokens
  case ts of
    (t : rest) | pred t -> do
      debugLog $ "Try consumed token (with layout): " <> T.pack (show t)
      return (Right (Just t, rest))
    _ -> do
      -- Log what's in the stream even when not consuming
      when (not (null ts)) $
        debugLog $ "Did NOT consume with tryConsumeWithLayout, next token: " <> T.pack (show (head ts))
      return (Right (Nothing, ts))

-- Helper to explicitly consume a layout token
consumeLayout :: (Token -> Bool) -> Parser (Maybe Token)
consumeLayout pred = Parser $ \ts ->
  case ts of
    (t : rest) | pred t -> do
      debugLog $ "Consumed layout token: " <> T.pack (show t)
      return (Right (Just t, rest))
    _ -> return (Right (Nothing, ts))

-- Helper to skip all empty lines and whitespace between statements
skipEmptyLines :: Parser ()
skipEmptyLines = Parser $ \ts ->
  let ts' = dropWhile (\t -> t == TNewline || t == TIndent || t == TDedent) ts
   in return (Right ((), ts'))

-- Parse many occurrences.
many :: Parser a -> Parser [a]
many p =
  ( do
      x <- p
      xs <- many p
      return (x : xs)
  )
    <|> return []

-- Enhanced peek with debugging info
peek :: Parser (Maybe Token)
peek = Parser $ \ts -> do
  let ts' = dropWhile isLayout ts
  case ts' of
    (t : _) -> do
      -- Add debug info for the peek operation
      debugLog $ "Peek found token: " <> T.pack (show t)
      return (Right (Just t, ts))
    [] -> do
      debugLog "Peek found end of input"
      return (Right (Nothing, ts))
  where
    isLayout t = case t of
      TIndent -> True
      TDedent -> True
      TNewline -> True
      _ -> False

-- Consume a token of a specific type directly (skipping layout tokens)
consumeToken :: Token -> Parser ()
consumeToken expected = do
  _ <- consume (== expected)
  return ()

-- Consume a token matching a predicate directly (skipping layout tokens)
consumeTokenMatching :: (Token -> Bool) -> Parser Token
consumeTokenMatching = consume

-- Helper to check if a token matches
isToken :: Token -> Token -> Bool
isToken expected actual = expected == actual

-- Helper for token types
isTokenOfType :: (Token -> Bool) -> Token -> Bool
isTokenOfType pred token = pred token

--------------------------------------------------------------------------------
-- Expression Parsers
--------------------------------------------------------------------------------

-- Complete rewrite of parseIf with step-by-step handling of tokens
parseIf :: Parser Expr
parseIf = do
  logInParser "Entering parseIf"

  -- Consume the 'if' keyword
  _ <- consume (== TKeyword (T.pack "if"))

  -- Parse the condition
  condition <- parseExpr

  -- Consume the colon
  _ <- consume (== TSymbol (T.pack ":"))

  -- Check if we have a block form by looking for newline
  hasNewline <- isNextToken TNewline

  thenExpr <- if hasNewline then do
    logInParser "Handling block if-then form"

    -- Consume newline and indent
    _ <- consumeToken TNewline
    _ <- consumeToken TIndent

    -- Parse block
    expr <- parseIndentedBlock

    -- Explicitly consume dedent
    _ <- many (consumeLayout (== TDedent))

    return expr
  else do
    logInParser "Handling inline if-then form"

    -- Parse a single expression (which could be a sequence)
    parseExpr

  -- Look for else clause
  hasElse <- isNextTokenOfType (\t -> case t of
                                 TKeyword k | k == T.pack "else" -> True
                                 _ -> False)

  if hasElse then do
    logInParser "Found else clause"

    -- Consume else and colon
    _ <- consumeToken (TKeyword (T.pack "else"))
    _ <- consumeToken (TSymbol (T.pack ":"))

    -- Check if else has block form
    hasElseNewline <- isNextToken TNewline

    elseExpr <- if hasElseNewline then do
      logInParser "Handling block else form"

      -- Consume newline and indent
      _ <- consumeToken TNewline
      _ <- consumeToken TIndent

      -- Parse block
      expr <- parseIndentedBlock

      -- Explicitly consume dedent
      _ <- many (consumeLayout (== TDedent))

      return expr
    else do
      logInParser "Handling inline else form"

      -- Parse a single expression (which could be a sequence)
      parseExpr

    -- Return the complete if-else expression
    return $ EIf condition thenExpr elseExpr
  else do
    logInParser "No else clause, using nil"
    return $ EIf condition thenExpr (ELit LNil)

-- Helper for parsing indented blocks (multiple statements)
parseIndentedBlock :: Parser Expr
parseIndentedBlock = do
  logInParser "Parsing indented block"

  -- Parse statements until dedent
  stmts <- parseStatementsUntilDedent

  -- Process statements into an expression
  case stmts of
    [] -> do
      logInParser "Empty block, returning nil"
      return (ELit LNil)
    [Expr e] -> do
      logInParser "Single expression block"
      return e
    _ -> do
      let (defs, exprs) = partitionDefs stmts
      let finalExpr = if null exprs then ELit LNil else last exprs
      logInParser "Multiple statements in block"
      return $ EApp (ELam [] (ExprBody defs finalExpr)) []

-- Helper to check if the next token is of a specific type without consuming it
isNextToken :: Token -> Parser Bool
isNextToken expected = do
  mTok <- peek
  return $ case mTok of
    Just tok -> tok == expected
    Nothing -> False

-- Helper to check if the next token satisfies a predicate without consuming it
isNextTokenOfType :: (Token -> Bool) -> Parser Bool
isNextTokenOfType pred = do
  mTok <- peek
  return $ case mTok of
    Just tok -> pred tok
    Nothing -> False

-- Parse basic expressions
parseExpr :: Parser Expr
parseExpr = do
  nextTok <- peek
  case nextTok of
    Just (TKeyword k) | k == T.pack "if" -> parseIf
    Just (TIdent i) | i == T.pack "unless" -> parseMacroUsage
    _ -> parseComparison  -- Direct to comparison, no sequence handling here

-- Parse a semicolon-separated sequence of expressions
parseExprSequence :: Parser Expr
parseExprSequence = do
  logInParser "Entering parseExprSequence"

  -- Parse the first expression
  firstExpr <- parseComparison

  -- Check if followed by semicolon
  hasSemicolon <- isNextToken (TSymbol (T.pack ";"))

  if hasSemicolon then do
    -- Consume the semicolon
    _ <- consumeToken (TSymbol (T.pack ";"))

    -- Parse the rest of the sequence
    restExpr <- parseExprSequence

    -- Return a lambda that evaluates expressions in sequence
    return $ EApp (ELam [T.pack "_"] (ExprBody [] restExpr)) [firstExpr]
  else do
    return firstExpr

-- Parse comparisons (lowest precedence)
parseComparison :: Parser Expr
parseComparison = do
  lhs <- parseAddSub
  (do
    op <- tryConsume (\t -> case t of
           TSymbol s -> s `elem` [T.pack "<", T.pack ">", T.pack "<=", T.pack ">=", T.pack "=="]
           _ -> False)
    case op of
      Just tok -> do
        rhs <- parseAddSub
        let opName = case tok of
              TSymbol s | s == T.pack "<=" -> T.pack "<="
              TSymbol s | s == T.pack ">=" -> T.pack ">="
              TSymbol s | s == T.pack "<" -> T.pack "<"
              TSymbol s | s == T.pack ">" -> T.pack ">"
              TSymbol s | s == T.pack "==" -> T.pack "eq?"
              _ -> error "Unknown comparison operator"
        return $ EApp (EBuiltinIdent opName) [lhs, rhs]
      Nothing -> return lhs
   ) <|> return lhs

-- Parse addition and subtraction (middle precedence)
parseAddSub :: Parser Expr
parseAddSub = do
  lhs <- parseMulDiv
  parseAddSubRest lhs
  where
    parseAddSubRest lhs =
      (do
        op <- tryConsume (\t -> case t of
               TSymbol s | s == T.pack "+" -> True
               TSymbol s | s == T.pack "-" -> True
               _ -> False)
        case op of
          Just (TSymbol s) | s == T.pack "+" -> do
            rhs <- parseMulDiv
            let addExpr = EApp (EBuiltinIdent (T.pack "+")) [lhs, rhs]
            parseAddSubRest addExpr
          Just (TSymbol s) | s == T.pack "-" -> do
            rhs <- parseMulDiv
            let subExpr = EApp (EBuiltinIdent (T.pack "-")) [lhs, rhs]
            parseAddSubRest subExpr
          _ -> return lhs
      ) <|> return lhs

-- Parse multiplication and division (highest precedence)
parseMulDiv :: Parser Expr
parseMulDiv = do
  lhs <- parseUnary
  parseMulDivRest lhs
  where
    parseMulDivRest lhs =
      (do
        op <- tryConsume (\t -> case t of
               TSymbol s | s == T.pack "*" -> True
               TSymbol s | s == T.pack "/" -> True
               _ -> False)
        case op of
          Just (TSymbol s) | s == T.pack "*" -> do
            rhs <- parseUnary
            let mulExpr = EApp (EBuiltinIdent (T.pack "*")) [lhs, rhs]
            parseMulDivRest mulExpr
          Just (TSymbol s) | s == T.pack "/" -> do
            rhs <- parseUnary
            let divExpr = EApp (EBuiltinIdent (T.pack "/")) [lhs, rhs]
            parseMulDivRest divExpr
          _ -> return lhs
      ) <|> return lhs

-- Parse unary operators like unary minus or not
parseUnary :: Parser Expr
parseUnary = do
  op <- tryConsume (\t -> case t of
         TSymbol s | s == T.pack "-" -> True
         TKeyword k | k == T.pack "not" -> True
         _ -> False)
  case op of
    Just (TSymbol s) | s == T.pack "-" -> do
      expr <- parseUnary
      return $ EApp (EBuiltinIdent (T.pack "-")) [ELit (LInt 0), expr]
    Just (TKeyword k) | k == T.pack "not" -> do
      expr <- parseUnary
      return $ EApp (EBuiltinIdent (T.pack "not")) [expr]
    Nothing -> parseCall

-- Parse function application with either space or parentheses syntax
parseCall :: Parser Expr
parseCall = do
  primary <- parsePrimary
  parseCallRest primary
  where
    parseCallRest fun =
      -- Try parentheses syntax with multiple arguments
      (do
          _ <- consume (== TSymbol (T.pack "("))
          args <- parseArgList
          _ <- consume (== TSymbol (T.pack ")"))
          let callExpr = EApp fun args

          -- IMPORTANT: Only recurse if we see another open paren
          mNextTok <- peek
          case mNextTok of
            Just (TSymbol s) | s == T.pack "(" -> parseCallRest callExpr
            _ -> return callExpr
      )
      <|>
      -- Try space-separated syntax for builtins
      (case fun of
          EBuiltinIdent _ -> do
            -- Check if the next token is suitable for an argument
            mNextTok <- peek
            case mNextTok of
              Just (TSymbol s) | s `elem` [T.pack ")", T.pack ",", T.pack ";", T.pack ":"] -> return fun
              Just TNewline -> return fun
              Just TDedent -> return fun
              Just TEOF -> return fun
              _ -> do
                arg <- parsePrimary
                let callExpr = EApp fun [arg]
                parseCallRest callExpr
          _ -> return fun
      )
      <|> (return fun)  -- Always provide a safe fallback

parsePrimary :: Parser Expr
parsePrimary = parseLambda <|> parseParens <|> parseMacroUsage <|> parseTermSimple
  where
    parseParens = do
      _ <- consume (== TSymbol (T.pack "("))
      e <- parseExpr
      _ <- consume (== TSymbol (T.pack ")"))
      return e

-- Parse a lambda expression (anonymous function)
parseLambda :: Parser Expr
parseLambda = do
  _ <- consume (== TSymbol (T.pack "("))
  _ <- consume (== TKeyword (T.pack "fn"))
  _ <- consume (== TSymbol (T.pack "("))
  args <- parseArgs
  _ <- consume (== TSymbol (T.pack ")"))
  _ <- consume (== TSymbol (T.pack ":"))
  body <- parseExpr
  _ <- consume (== TSymbol (T.pack ")"))
  return (ELam args (ExprBody [] body))

-- Parse a macro definition
parseMacroDef :: Parser ExprBodyExpr
parseMacroDef = do
  logInParser "Entering parseMacroDef"

  -- Consume the 'macro' token
  _ <- consume (== TIdent (T.pack "macro"))

  -- Parse macro name
  nameToken <- consumeTokenMatching (\t -> case t of TIdent _ -> True; _ -> False)
  let macroName = case nameToken of TIdent n -> n; _ -> T.empty

  -- Parse parameter list
  _ <- consume (== TSymbol (T.pack "("))
  params <- parseArgs
  _ <- consume (== TSymbol (T.pack ")"))

  -- Consume colon
  _ <- consume (== TSymbol (T.pack ":"))

  -- Check if we have a block form
  hasNewline <- isNextToken TNewline

  bodyExpr <- if hasNewline then do
    -- Consume newline and indent
    _ <- consumeToken TNewline
    _ <- consumeToken TIndent

    -- Parse the body (an indented block)
    expr <- parseIndentedBlock

    return expr
  else do
    -- Inline form
    parseExpr

  -- Create macro definition
  let macroParamNames = map (\i -> T.pack ("$" ++ show i)) [1 .. length params]
      macroImpl = ELam macroParamNames (ExprBody [] bodyExpr)
      macroLambda = ELam [macroName] (ExprBody [] macroImpl)

  -- Return definition bound to "macro"
  return (Def (T.pack "macro") macroLambda)

-- Parse a macro usage expression (unless(condition): body)
parseMacroUsage :: Parser Expr
parseMacroUsage = do
  macroName <- consume (\t -> case t of TIdent _ -> True; _ -> False)
  let name = case macroName of TIdent n -> n; _ -> T.empty
  _ <- consume (== TSymbol (T.pack "("))
  arg1 <- parseExpr
  _ <- consume (== TSymbol (T.pack ")"))
  _ <- consume (== TSymbol (T.pack ":"))
  arg2 <- parseExpr
  return (EApp (EVar name) [arg1, arg2])

-- Parse a simple term: identifier or number.
parseTermSimple :: Parser Expr
parseTermSimple = do
  t <- consume isTerm
  case t of
    TIdent name ->
      if (isBuiltin name)
        then return (EBuiltinIdent name)
        else return (EVar name)
    TNumber n -> return (ELit (LInt n))
    TString s -> return (ELit (LString s))
    TKeyword k | k == T.pack "nil" -> return (ELit LNil)
    TKeyword k | k == T.pack "true" -> return (ELit (LBool True))
    TKeyword k | k == T.pack "false" -> return (ELit (LBool False))
    _ -> Parser $ \_ -> return (Left "Expected a term")
  where
    isTerm tok = case tok of
      TIdent _ -> True
      TNumber _ -> True
      TString _ -> True
      TKeyword k -> k `elem` [T.pack "nil", T.pack "true", T.pack "false"]
      _ -> False

--------------------------------------------------------------------------------
-- Top-Level Statement Parsers
--------------------------------------------------------------------------------

-- Enhanced statement parsing with detailed logging and correct handling of DEDENTs
parseStatementsUntilDedent :: Parser [ExprBodyExpr]
parseStatementsUntilDedent = do
  logInParser "Entering parseStatementsUntilDedent"

  -- Check if we're at a DEDENT token
  isDedent <- isNextToken TDedent
  if isDedent then do
    logInParser "Found DEDENT, ending block"
    -- Consume the DEDENT token
    _ <- consumeToken TDedent
    return []
  else do
    -- Check for different statement types
    nextTok <- peek

    stmt <- case nextTok of
      -- Handle let binding inside a block
      Just (TIdent i) | i == T.pack "let" -> do
        logInParser "Found let binding in block"
        -- Consume let token
        _ <- consumeToken (TIdent (T.pack "let"))

        -- Parse variable name
        nameToken <- consumeTokenMatching (\t -> case t of TIdent _ -> True; _ -> False)
        let name = case nameToken of TIdent n -> n; _ -> T.empty

        -- Consume equals sign
        _ <- consumeToken (TSymbol (T.pack "="))

        -- Parse the expression
        expr <- parseExpr

        -- Return definition
        return $ Def name expr

      -- For other statement types
      _ -> parseTopLevelStmt

    -- Skip newlines
    _ <- many (consumeLayout (== TNewline))

    -- Parse remaining statements
    rest <- parseStatementsUntilDedent

    -- Return the statement and the rest
    return (stmt : rest)

-- Parse a comma-separated list of arguments (identifiers).
parseArgs :: Parser [Text]
parseArgs =
  ( do
      argToken <- consume (\t -> case t of TIdent _ -> True; _ -> False)
      let arg = case argToken of TIdent a -> a; _ -> T.empty
      rest <-
        many
          ( do
              _ <- consume (== TSymbol (T.pack ","))
              tok <- consume (\t -> case t of TIdent _ -> True; _ -> False)
              return (case tok of TIdent a -> a; _ -> T.empty)
          )
      return (arg : rest)
  )
    <|> return []

-- Parse a comma-separated list of expressions for function arguments
parseArgList :: Parser [Expr]
parseArgList =
  ( do
      firstArg <- parseExpr
      restArgs <-
        many
          ( do
              _ <- consume (== TSymbol (T.pack ","))
              parseExpr
          )
      return (firstArg : restArgs)
  )
  <|> return []

-- | Parse a function definition of the form:
--    fn <name>(<args>): <body>
parseFnDef :: Parser ExprBodyExpr
parseFnDef = do
  logInParser "Entering parseFnDef"
  _ <- consume (== TKeyword (T.pack "fn"))
  nameToken <- consume (\t -> case t of TIdent _ -> True; _ -> False)
  let name = case nameToken of TIdent n -> n; _ -> T.empty
  _ <- consume (== TSymbol (T.pack "("))
  args <- parseArgs
  _ <- consume (== TSymbol (T.pack ")"))
  _ <- consume (== TSymbol (T.pack ":"))
  bodyExpr <- parseExpr
  -- Wrap the function body in a lambda with no definitions.
  return (Def name (ELam args (ExprBody [] bodyExpr)))

-- Parse a top-level let binding: let name = expr
parseTopLevelLet :: Parser ExprBodyExpr
parseTopLevelLet = do
  logInParser "Entering parseTopLevelLet"

  -- Consume the 'let' token
  _ <- consume (== TIdent (T.pack "let"))

  -- Parse the variable name
  nameToken <- consumeTokenMatching (\t -> case t of TIdent _ -> True; _ -> False)
  let name = case nameToken of TIdent n -> n; _ -> T.empty

  -- Consume equals sign
  _ <- consume (== TSymbol (T.pack "="))

  -- Parse the expression
  expr <- parseExpr

  -- Return a definition
  return (Def name expr)

-- Parse a single statement without top-level concerns
parseTopLevelStmt :: Parser ExprBodyExpr
parseTopLevelStmt =
  parseMacroDef <|> parseTopLevelLet <|> parseFnDef <|> parseExprStmt
  where
    parseExprStmt = do
      e <- parseExpr
      return (Expr e)

-- | Parse a sequence of top-level statements.
parseProgramExprs :: Parser [ExprBodyExpr]
parseProgramExprs = do
  _ <- skipEmptyLines
  parseMultipleTopStmts

-- Parse multiple top-level statements separated by empty lines
parseMultipleTopStmts :: Parser [ExprBodyExpr]
parseMultipleTopStmts = do
  logInParser "Entering parseMultipleTopStmts"

  -- Check if we've reached end of input
  isEOF <- isNextToken TEOF

  if isEOF then do
    logInParser "Reached end of file, returning empty list"
    return []
  else do
    -- Parse a single statement first
    logInParser "Parsing top-level statement"
    stmt <- parseTopLevelStmt

    -- Check for a semicolon which indicates multiple statements on one line
    hasSemicolon <- isNextToken (TSymbol (T.pack ";"))

    if hasSemicolon then do
      logInParser "Found semicolon, parsing next statement on same line"
      -- Consume the semicolon
      _ <- consumeToken (TSymbol (T.pack ";"))

      -- Parse the rest of the statements (after the semicolon)
      rest <- parseMultipleTopStmts

      -- Return this statement and the rest
      return (stmt : rest)
    else do
      -- No semicolon, skip any trailing newlines/empty lines
      _ <- skipEmptyLines

      -- Continue parsing the rest of the top-level statements
      rest <- parseMultipleTopStmts

      -- Return this statement and the rest
      return (stmt : rest)

--------------------------------------------------------------------------------
-- Helpers to Combine Top-Level Statements
--------------------------------------------------------------------------------

-- Partition a list of top-level statements into definitions and expressions.
partitionDefs :: [ExprBodyExpr] -> ([ExprBodyExpr], [Expr])
partitionDefs = foldr f ([], [])
  where
    f (Def n e) (defs, exprs) = (Def n e : defs, exprs)
    f (Expr e) (defs, exprs) = (defs, e : exprs)

-- Combine a list of top-level statements into a single AST.
combineStmts :: [ExprBodyExpr] -> Expr
combineStmts stmts =
  let (defs, exprs) = partitionDefs stmts
   in case exprs of
        -- Handle the case where there are only function definitions (no expressions)
        []
          | not (null defs) ->
              let mainExpr = ELit LNil -- Implicit nil result
               in EApp (ELam [] (ExprBody defs mainExpr)) []
        -- If there are truly no statements at all, this is an error
        [] -> error "No final expression in program"
        -- Standard case: one or more expressions
        [e] -> EApp (ELam [] (ExprBody defs e)) [] -- Single expression case
        _ ->
          let finalExpr = last exprs
              preceding = init exprs
              -- Generate proper sequence with exit continuations
              chainedExprs = foldr1 makeSequenceWithExit preceding
              finalSequence = makeSequenceWithExit chainedExprs finalExpr
           in EApp (ELam [] (ExprBody defs finalSequence)) []
  where
    -- Chain expressions with continuations that don't depend on earlier results
    makeSequenceWithExit e1 e2 =
      EApp (ELam [T.pack "_"] (ExprBody [] e2)) [e1]

--------------------------------------------------------------------------------
-- Top-Level parseProgram Function
--------------------------------------------------------------------------------

dropLayout :: [Token] -> [Token]
dropLayout = filter (not . isLayout)
  where
    isLayout t = case t of
      TIndent -> True
      TDedent -> True
      TNewline -> True
      _ -> False

-- Add a version that takes the debug flag
parseWithDebug :: Bool -> Text -> IO (Either String Expr)
parseWithDebug debugMode input = do
  tokens <- lexZayin debugMode input
  runStderrLoggingT $ runReaderT (parseAction tokens) debugMode
  where
    parseAction tokens = do
      debugLog $ "Tokens after layout: " <> T.pack (show tokens)

      -- Log critical token information
      forceDebugLog $ "PARSING PROGRAM WITH " ++ show (length tokens) ++ " TOKENS"
      forceDebugLog $ "FIRST 20 TOKENS: " ++ show (take 20 tokens)

      res <- runParser parseProgramExprs tokens
      case res of
        Right (stmts, remaining) ->
          if dropLayout remaining == [TEOF]
            then do
              let progAst = combineStmts stmts
              debugLog $ "Parsing complete: " <> T.pack (show progAst)
              return (Right progAst)
            else do
              debugLog $ "Unconsumed tokens: " <> T.pack (show remaining)
              return (Left ("Parsing error: unconsumed tokens " ++ show remaining))
        Left err -> do
          forceDebugLog $ "PARSE ERROR: " ++ err
          return (Left err)

-- Updated top-level API for backward compatibility
parseProgram :: Text -> IO (Either String Expr)
parseProgram = parseWithDebug False
