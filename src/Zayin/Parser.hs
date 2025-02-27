{-# LANGUAGE OverloadedStrings #-}

module Zayin.Parser (parseProgram, parseWithDebug) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logDebugN, runStderrLoggingT)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
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
                    case T.breakOn "\"" rest of
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
                    let (word, rest') = T.span isAlphaNum t'
                        token = if isKeyword word then TKeyword word else TIdent word
                     in token : nextToken rest'
                | isPunctuation c ->
                    TSymbol (T.singleton c) : nextToken rest
                | otherwise -> nextToken rest

isPunctuation :: Char -> Bool
isPunctuation c = c `elem` ("()+-*/=,:;" :: String)

isKeyword :: Text -> Bool
isKeyword w = w `elem` ["if", "then", "else", "fn", "where", "do", "true", "false", "nil", "not"]

isBuiltin :: Text -> Bool
isBuiltin w = w `elem` ["display", "not"]

--------------------------------------------------------------------------------
-- Expression AST Transformation for Variable Renaming in Macros
--------------------------------------------------------------------------------

-- Rename variables in an expression based on a mapping
renameVarsInExpr :: Map Text Text -> Expr -> Expr
renameVarsInExpr varMap expr = case expr of
  EVar name -> case Map.lookup name varMap of
    Just newName -> EVar newName
    Nothing -> EVar name
  ELit lit -> ELit lit
  EBuiltinIdent name -> EBuiltinIdent name
  EIf cond thenE elseE ->
    EIf
      (renameVarsInExpr varMap cond)
      (renameVarsInExpr varMap thenE)
      (renameVarsInExpr varMap elseE)
  ESet name value ->
    ESet name (renameVarsInExpr varMap value)
  ELet bindings body ->
    let renamedBindings = [(name, renameVarsInExpr varMap val) | (name, val) <- bindings]
        renamedBody = renameVarsInExprBody varMap body
     in ELet renamedBindings renamedBody
  ELam params body ->
    -- Don't rename variables shadowed by lambda parameters
    let filteredMap = foldr Map.delete varMap params
        renamedBody = renameVarsInExprBody filteredMap body
     in ELam params renamedBody
  EApp func args ->
    EApp (renameVarsInExpr varMap func) (map (renameVarsInExpr varMap) args)

-- Rename variables in an expression body
renameVarsInExprBody :: Map Text Text -> ExprBody -> ExprBody
renameVarsInExprBody varMap (ExprBody exprs finalExpr) =
  let renamedExprs = map (renameVarsInBodyExpr varMap) exprs
      renamedFinal = renameVarsInExpr varMap finalExpr
   in ExprBody renamedExprs renamedFinal

-- Rename variables in a body expression
renameVarsInBodyExpr :: Map Text Text -> ExprBodyExpr -> ExprBodyExpr
renameVarsInBodyExpr varMap bodyExpr = case bodyExpr of
  Def name expr -> Def name (renameVarsInExpr varMap expr)
  Expr expr -> Expr (renameVarsInExpr varMap expr)

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

-- Consume a token (skipping layout tokens).
consume :: (Token -> Bool) -> Parser Token
consume pred = Parser $ \ts ->
  let ts' = dropWhile isLayout ts
   in case ts' of
        (t : rest) | pred t -> do
          debugLog $ "Consumed token: " <> T.pack (show t)
          return (Right (t, rest))
        (t : _) -> do
          debugLog $ "Failed to consume token, found: " <> T.pack (show t)
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

-- Try to consume a token without failing if not found
tryConsume :: (Token -> Bool) -> Parser (Maybe Token)
tryConsume pred = Parser $ \ts ->
  let ts' = dropWhile isLayout ts
   in case ts' of
        (t : rest) | pred t -> do
          debugLog $ "Try consumed token: " <> T.pack (show t)
          return (Right (Just t, rest))
        _ -> return (Right (Nothing, ts))
  where
    isLayout t = case t of
      TIndent -> True
      TDedent -> True
      TNewline -> True
      _ -> False

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

-- Look ahead at the next token without consuming it
peek :: Parser (Maybe Token)
peek = Parser $ \ts ->
  let ts' = dropWhile isLayout ts
   in case ts' of
        (t : _) -> return (Right (Just t, ts))
        [] -> return (Right (Nothing, ts))
  where
    isLayout t = case t of
      TIndent -> True
      TDedent -> True
      TNewline -> True
      _ -> False

--------------------------------------------------------------------------------
-- Top-Level Statement Parsers Producing ExprBodyExpr
--------------------------------------------------------------------------------

-- A top-level statement is a macro definition, function definition, or a plain expression.
parseTopStmt :: Parser ExprBodyExpr
parseTopStmt = parseMacroDef <|> parseFnDef <|> parseLetStmt <|> parseExprWithSemicolon
  where
    parseExprWithSemicolon = do
      e <- parseExprList -- Use parseExprList to handle semicolon-separated expressions
      return (Expr e)

    -- Parse a let binding as a top-level statement
    parseLetStmt = do
      letExpr <- parseLet
      return (Expr letExpr)

-- | Parse a macro definition of the form:
--    macro name(param1, param2): <body>
parseMacroDef :: Parser ExprBodyExpr
parseMacroDef = do
  tok <- peek
  case tok of
    Just (TIdent "macro") -> do
      _ <- consume (== TIdent "macro")
      nameToken <- consume (\t -> case t of TIdent _ -> True; _ -> False)
      let macroName = case nameToken of TIdent n -> n; _ -> ""
      _ <- consume (== TSymbol "(")
      params <- parseArgs
      _ <- consume (== TSymbol ")")
      _ <- consume (== TSymbol ":")

      -- Try to parse indented block for macro body - with parameter mapping
      bodyExpr <- parseIndentedExpr params

      -- Skip any trailing newlines
      skipEmptyLines

      -- Create the macro definition AST node
      -- First define a lambda with the macro parameters
      let macroParamNames = map (\i -> "$" <> T.pack (show i)) [1 .. length params]
      let macroImpl = ELam macroParamNames (ExprBody [] bodyExpr)

      -- Wrap in a lambda that takes the macro name
      let macroLambda = ELam [macroName] (ExprBody [] macroImpl)

      -- Return as a definition bound to "macro"
      return (Def "macro" macroLambda)
    _ -> Parser $ \_ -> return (Left "Not a macro definition")

-- Parse an indented expression (used for macro body) with parameter renaming
parseIndentedExpr :: [Text] -> Parser Expr
parseIndentedExpr params = Parser $ \ts -> do
  -- We need to look for an indented expression that starts with "if not"
  -- Find the next line after the newline
  let skipNewlines = dropWhile (== TNewline) ts
  case skipNewlines of
    (TIndent : rest) -> do
      case dropWhile isLayout rest of
        (TKeyword "if" : _) -> do
          debugLog "Found indented if-expression for macro body"
          -- First parse the expression normally
          result <- runParser parseIf rest
          case result of
            Left err -> return (Left err)
            Right (expr, remainingTokens) -> do
              -- Create variable renaming map from parameters to $1, $2, etc.
              let paramMap = Map.fromList (zip params (map (\i -> "$" <> T.pack (show i)) [1 .. length params]))
              -- Rename all variables in the expression according to the mapping
              let renamedExpr = renameVarsInExpr paramMap expr
              debugLog $ "Renamed variables in macro body: " <> T.pack (show renamedExpr)
              return (Right (renamedExpr, remainingTokens))
        _ -> return (Left "Expected if-expression in macro body")
    _ -> return (Left "Expected indented block for macro body")
  where
    isLayout t = case t of
      TIndent -> True
      TDedent -> True
      TNewline -> True
      _ -> False

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
parseArgs =
  ( do
      argToken <- consume (\t -> case t of TIdent _ -> True; _ -> False)
      let arg = case argToken of TIdent a -> a; _ -> ""
      rest <-
        many
          ( do
              _ <- consume (== TSymbol ",")
              tok <- consume (\t -> case t of TIdent _ -> True; _ -> False)
              return (case tok of TIdent a -> a; _ -> "")
          )
      return (arg : rest)
  )
    <|> return []

-- | Parse a let binding with two possible formats:
--    1. let <name> = <expr> [, <name> = <expr>]*
--       <body>
--    2. let
--       <name> = <expr>
--       <name> = <expr>
--       ...
--       <body>
parseLet :: Parser Expr
parseLet = do
  _ <- consume (== TIdent "let")

  -- Check if we have an immediate newline (format 2)
  mNewline <- tryConsume (== TNewline)

  bindings <- case mNewline of
    -- Format 2: Bindings on separate lines
    Just _ -> do
      -- Expect indentation for the bindings block
      _ <- consumeLayout (== TIndent)

      -- Parse multiple bindings, each on a separate line
      binds <- many1 parseBindingLine

      -- Expect dedentation after bindings
      _ <- consumeLayout (== TDedent)

      return binds

    -- Format 1: Bindings on same line separated by commas
    Nothing -> do
      -- Parse the first binding
      firstBinding <- parseBinding

      -- Check for additional bindings separated by commas
      moreBindings <-
        many
          ( do
              _ <- tryConsume (== TSymbol ",")
              parseBinding
          )

      -- Expect a newline after the bindings
      _ <- consumeLayout (== TNewline)

      return (firstBinding : moreBindings)

  -- Parse the body expressions
  bodyExprs <- parseMultipleTopStmts

  -- Transform the body expressions into a proper body
  case bodyExprs of
    [] -> return (ELet bindings (ExprBody [] (ELit LNil)))
    [Expr e] -> return (ELet bindings (ExprBody [] e))
    _ ->
      let (defs, exprs) = partitionDefs bodyExprs
          finalExpr =
            if null exprs
              then ELit LNil
              else last exprs
       in return (ELet bindings (ExprBody defs finalExpr))

-- Helper to parse a binding on a single line with no comma
parseBindingLine :: Parser (T.Text, Expr)
parseBindingLine = do
  binding <- parseBinding
  _ <- consumeLayout (== TNewline)
  return binding

-- Helper to parse a single binding (name = expr)
parseBinding :: Parser (T.Text, Expr)
parseBinding = do
  nameToken <- consume (\t -> case t of TIdent _ -> True; _ -> False)
  let name = case nameToken of TIdent n -> n; _ -> ""
  _ <- consume (== TSymbol "=")
  expr <- parseExpr
  return (name, expr)

-- Helper for parsing at least one occurrence of something
many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x : xs)

-- | Parse a sequence of top-level statements.
parseProgramExprs :: Parser [ExprBodyExpr]
parseProgramExprs = do
  _ <- skipEmptyLines
  result <- parseMultipleTopStmts
  _ <- skipEmptyLines
  return result

-- Parse multiple top-level statements separated by empty lines
parseMultipleTopStmts :: Parser [ExprBodyExpr]
parseMultipleTopStmts = do
  -- Try to parse a statement
  mTok <- peek
  case mTok of
    Just TEOF -> return [] -- End of input
    Just _ -> do
      -- Parse one statement
      stmt <- parseTopStmt
      -- Skip any trailing newlines/empty lines
      _ <- skipEmptyLines
      -- Parse more statements
      rest <- parseMultipleTopStmts
      return (stmt : rest)
    Nothing -> return [] -- No more tokens

--------------------------------------------------------------------------------
-- Expression Parsers Producing Our Actual AST
--------------------------------------------------------------------------------

-- Parse a lambda expression (anonymous function)
parseLambda :: Parser Expr
parseLambda = do
  _ <- consume (== TSymbol "(")
  _ <- consume (== TKeyword "fn")
  _ <- consume (== TSymbol "(")
  args <- parseArgs
  _ <- consume (== TSymbol ")")
  _ <- consume (== TSymbol ":")
  body <- parseExpr
  _ <- consume (== TSymbol ")")
  return (ELam args (ExprBody [] body))

-- Parse a macro usage expression (unless(condition): body)
parseMacroUsage :: Parser Expr
parseMacroUsage = do
  macroName <- consume (\t -> case t of TIdent _ -> True; _ -> False)
  let name = case macroName of TIdent n -> n; _ -> ""
  _ <- consume (== TSymbol "(")
  arg1 <- parseExpr
  _ <- consume (== TSymbol ")")
  _ <- consume (== TSymbol ":")
  arg2 <- parseExpr
  return (EApp (EVar name) [arg1, arg2])

-- Expressions include if-expressions, addition, function calls, and primaries.
parseExpr :: Parser Expr
parseExpr = do
  nextTok <- peek
  case nextTok of
    Just (TKeyword "if") -> parseIf
    Just (TIdent "unless") -> parseMacroUsage
    Just (TIdent "let") -> parseLet
    _ -> parseAdd >>= parseExprWithCall
  where
    parseExprWithCall expr =
      ( do
          _ <- tryConsume (== TSymbol "(")
          arg <- parseExpr
          _ <- consume (== TSymbol ")")
          let callExpr = EApp expr [arg]
          -- Check for additional function applications
          parseExprWithCall callExpr
      )
        <|> return expr

-- Parse expressions separated by semicolons for top-level statements
parseExprList :: Parser Expr
parseExprList = do
  -- Parse the first expression
  first <- parseExpr

  -- See if there's a semicolon followed by more expressions
  semi <- tryConsume (== TSymbol ";")
  case semi of
    Just _ -> do
      -- Parse the rest of the expressions
      rest <- parseExprList
      -- Return a sequence where expressions are evaluated in order
      return $ EApp (ELam ["_"] (ExprBody [] rest)) [first]
    Nothing -> return first

-- Parse a list of expressions separated by semicolons
parseIf :: Parser Expr
parseIf = do
  _ <- consume (== TKeyword "if")

  -- Support for 'not' operator within the condition
  mNot <- tryConsume (== TKeyword "not")
  cond <- parseExpr

  -- Apply 'not' if present
  let finalCond = case mNot of
        Just _ -> EApp (EBuiltinIdent "not") [cond]
        Nothing -> cond

  _ <- consume (== TSymbol ":")
  thenE <- parseExpr

  -- Parse optional 'else' clause
  mElse <- tryConsume (== TKeyword "else")
  case mElse of
    Just _ -> do
      _ <- consume (== TSymbol ":")
      elseE <- parseExpr
      return (EIf finalCond thenE elseE)
    Nothing -> return (EIf finalCond thenE (ELit LNil))

-- Helper for parsing separated lists
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep =
  ( do
      first <- p
      rest <-
        many
          ( do
              _ <- sep
              p
          )
      return (first : rest)
  )
    <|> return []

-- Parse left-associative addition.
parseAdd :: Parser Expr
parseAdd = do
  lhs <- parseCall
  moreAdd lhs
  where
    moreAdd lhs =
      ( do
          _ <- consume (== TSymbol "+")
          rhs <- parseCall
          let addExpr = EApp (EBuiltinIdent "+") [lhs, rhs]
          moreAdd addExpr
      )
        <|> return lhs

-- Parse function application with either space or parentheses syntax
parseCall :: Parser Expr
parseCall = do
  primary <- parsePrimary
  parseCall' primary
  where
    parseCall' fun =
      -- Try parentheses syntax first
      ( do
          _ <- consume (== TSymbol "(")
          arg <- parseExpr
          _ <- consume (== TSymbol ")")
          let callExpr = EApp fun [arg]
          parseCall' callExpr
      )
        <|>
        -- Try space-separated syntax for builtins
        ( case fun of
            EBuiltinIdent _ -> do
              arg <- parsePrimary -- Use parsePrimary to avoid left recursion
              let callExpr = EApp fun [arg]
              parseCall' callExpr
            _ -> return fun
        )
        <|> return fun

-- Parse a primary expression.
parsePrimary :: Parser Expr
parsePrimary = parseLambda <|> parseParens <|> parseTermSimple
  where
    parseParens = do
      _ <- consume (== TSymbol "(")
      e <- parseExpr
      _ <- consume (== TSymbol ")")
      return e

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
    TString s -> return (ELit (LString s)) -- Handle string literals
    TKeyword "nil" -> return (ELit LNil)
    TKeyword "true" -> return (ELit (LBool True))
    TKeyword "false" -> return (ELit (LBool False))
    _ -> Parser $ \_ -> return (Left "Expected a term")
  where
    isTerm tok = case tok of
      TIdent _ -> True
      TNumber _ -> True
      TString _ -> True
      TKeyword k -> k `elem` ["nil", "true", "false"]
      _ -> False

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
      EApp (ELam ["_"] (ExprBody [] e2)) [e1]

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
        Left err -> return (Left err)

-- Updated top-level API for backward compatibility
parseProgram :: Text -> IO (Either String Expr)
parseProgram = parseWithDebug False
