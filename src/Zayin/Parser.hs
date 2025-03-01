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

-- Enhanced logger specifically for let statements
letDebugLog :: String -> ParserM ()
letDebugLog msg = do
  logInfoN $ T.pack $ "LET-DEBUG: " ++ msg

-- Debug logging that works within Parser monad
logInParser :: String -> Parser ()
logInParser msg = Parser $ \ts -> do
  forceDebugLog msg
  return (Right ((), ts))

-- Let-specific logging that works within Parser monad
letLogInParser :: String -> Parser ()
letLogInParser msg = Parser $ \ts -> do
  letDebugLog msg
  return (Right ((), ts))

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
isKeyword w = w `elem` ["if", "then", "else", "fn", "where", "do", "true", "false", "nil", "not"]

isBuiltin :: Text -> Bool
isBuiltin w = w `elem` ["display", "not", "gc_collect", "gc_stats", "spawn", "cons",
                        "+", "-", "*", "/", "%", "<", ">", "<=", ">=", "eq?", "car", "cdr"]

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

-- Examine and log the token stream - useful for debugging
inspectTokens :: Parser [Token]
inspectTokens = Parser $ \ts -> do
  forceDebugLog $ "INSPECTING TOKENS: " ++ show (take 10 ts)
  return (Right (ts, ts))

--------------------------------------------------------------------------------
-- Top-Level Statement Parsers Producing ExprBodyExpr
--------------------------------------------------------------------------------

-- Parse a let statement at the top level
parseLet :: Parser [ExprBodyExpr]
parseLet = do
  logInParser "Entering parseLet for top-level let"
  _ <- consume (== TIdent "let")
  (name, expr) <- parseBinding
  logInParser $ "Parsed top-level let binding: " ++ T.unpack name
  return [Def name expr]

-- Direct let parsing specifically for blocks, with comprehensive logging
parseDirectBlockLet :: Parser ExprBodyExpr
parseDirectBlockLet = Parser $ \tokens -> do
  forceDebugLog $ "DIRECT BLOCK LET: Initial tokens: " ++ show (take 5 tokens)

  -- For each token operation, directly use runParser and check results to avoid nested errors
  letResult <- runParser (consume (== TIdent "let")) tokens
  case letResult of
    Left err -> do
      forceDebugLog $ "FAILED to consume 'let' token: " ++ err
      return (Left err)
    Right (_, tokens1) -> do
      forceDebugLog "Successfully consumed 'let' token"

      -- Get next token in the stream
      peekResult <- runParser peek tokens1
      case peekResult of
        Right (Just nextToken, _) -> forceDebugLog $ "Next token: " ++ show nextToken
        _ -> forceDebugLog "No next token"

      -- Try to parse the variable name
      nameResult <- runParser (consume (\t -> case t of TIdent _ -> True; _ -> False)) tokens1
      case nameResult of
        Left err -> do
          forceDebugLog $ "FAILED to parse variable name: " ++ err
          return (Left err)
        Right (nameToken, tokens2) -> do
          let name = case nameToken of TIdent n -> n; _ -> ""
          forceDebugLog $ "Successfully parsed variable name: " ++ T.unpack name

          -- Directly examine the next token
          forceDebugLog $ "Before equals sign, tokens: " ++ show (take 3 tokens2)

          -- Handle equals sign
          eqResult <- runParser (consume (== TSymbol "=")) tokens2
          case eqResult of
            Left err -> do
              forceDebugLog $ "CRITICAL FAILURE: Could not parse equals sign: " ++ err
              -- Try to see what the problem is
              let actualToken = if null tokens2 then "NO TOKENS" else show (head tokens2)
              forceDebugLog $ "Current token: " ++ actualToken

              -- Give up with detailed error
              return (Left $ "Failed to parse equals sign after variable name: " ++ T.unpack name)

            Right (_, tokens3) -> do
              forceDebugLog "Successfully parsed equals sign"

              -- Now try to parse the expression
              exprResult <- runParser parseExpr tokens3
              case exprResult of
                Left err -> do
                  forceDebugLog $ "Failed to parse expression: " ++ err
                  return (Left err)
                Right (expr, tokens4) -> do
                  forceDebugLog $ "Successfully parsed expression: " ++ show expr
                  return (Right (Def name expr, tokens4))

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

-- Parse a comma-separated list of expressions for function arguments
parseArgList :: Parser [Expr]
parseArgList =
  ( do
      firstArg <- parseExpr
      restArgs <-
        many
          ( do
              _ <- consume (== TSymbol ",")
              parseExpr
          )
      return (firstArg : restArgs)
  )
  <|> return []

-- Define a parseBlock function that handles indented blocks
parseBlock :: Parser Expr
parseBlock = do
  logInParser "Entering parseBlock"
  -- Expect an indentation for the block
  _ <- consumeLayout (== TIndent)

  -- Parse all statements in the block
  stmts <- parseStatementsUntilDedent

  -- Log what we found
  logInParser $ "Block statements parsed: " ++ show (length stmts)

  -- Convert the statements into a proper expression
  case stmts of
    [] -> do
      logInParser "Empty block, returning nil"
      return (ELit LNil)  -- Empty block
    [Expr e] -> do
      logInParser "Single expression block"
      return e      -- Single expression, no need for wrapping
    _ -> do
      -- Multiple statements need to be wrapped in a lambda
      let (defs, exprs) = partitionDefs stmts
      let finalExpr = if null exprs then ELit LNil else last exprs
      logInParser $ "Complex block with " ++ show (length defs) ++ " defs and " ++ show (length exprs) ++ " exprs"
      return $ EApp (ELam [] (ExprBody defs finalExpr)) []

-- Enhanced statement parsing with detailed logging
parseStatementsUntilDedent :: Parser [ExprBodyExpr]
parseStatementsUntilDedent = do
  logInParser "Entering parseStatementsUntilDedent"

  -- First look specifically for a DEDENT token without skipping layout tokens
  isDedent <- checkForDedent
  if isDedent then do
    logInParser "Found DEDENT token, ending block"
    -- Consume the dedent and return empty list
    _ <- consumeLayout (== TDedent)
    return []
  else do
    -- Not at a dedent, check what kind of token we have
    mTok <- peek
    case mTok of
      Nothing -> do
        logInParser "No tokens left, ending block"
        return []  -- End of input

      Just (TIdent "let") -> do
        logInParser "Found 'let' in block, special handling activated"

        -- Consume 'let' token
        _ <- consume (== TIdent "let")

        -- Parse variable name
        nameToken <- consume (\t -> case t of TIdent _ -> True; _ -> False)
        let name = case nameToken of TIdent n -> n; _ -> ""
        logInParser $ "Parsed let variable name: " ++ T.unpack name

        -- Consume equals sign
        _ <- consume (== TSymbol "=")
        logInParser "Successfully consumed equals sign"

        -- Parse expression after equals
        expr <- parseExpr
        logInParser "Successfully parsed let expression value"

        -- Skip newlines and parse rest of block
        _ <- many (consumeLayout (== TNewline))
        rest <- parseStatementsUntilDedent

        -- Return definition + rest of statements
        return (Def name expr : rest)

      Just _ -> do
        -- Parse a regular statement (non-let)
        stmt <- parseTopLevelStmt
        logInParser "Parsed regular statement in block"

        -- Skip newlines
        _ <- many (consumeLayout (== TNewline))

        -- Parse rest of statements
        rest <- parseStatementsUntilDedent

        return (stmt : rest)

-- Helper to check for a DEDENT token without consuming it
checkForDedent :: Parser Bool
checkForDedent = Parser $ \ts -> do
  -- Only look at the first token
  let result = case ts of
        (TDedent:_) -> True
        _ -> False
  forceDebugLog $ "checkForDedent result: " ++ show result ++ " from token: " ++
      (if null ts then "EMPTY" else show (head ts))
  return (Right (result, ts))

-- Parse a let statement inside a block with enhanced debugging
parseBlockLet :: Parser ExprBodyExpr
parseBlockLet = Parser $ \tokens -> do
  debugLog "*** ENTERING parseBlockLet ***"
  debugLog $ "Initial tokens: " <> T.pack (show (take 10 tokens))

  -- Consume the "let" token
  letResult <- runParser (consume (== TIdent "let")) tokens
  case letResult of
    Left err -> return (Left $ "Failed to consume 'let': " ++ err)
    Right (letToken, tokens1) -> do
      debugLog $ "Successfully consumed let token: " <> T.pack (show letToken)

      -- Show next token before parsing name
      nextTokens1 <- runParser peek tokens1
      case nextTokens1 of
        Right (mToken, _) -> debugLog $ "Next token before parsing var name: " <> T.pack (show mToken)
        _ -> return ()

      -- Parse name
      nameResult <- runParser (consume (\t -> case t of TIdent _ -> True; _ -> False)) tokens1
      case nameResult of
        Left err -> return (Left $ "Failed to parse variable name: " ++ err)
        Right (nameToken, tokens2) -> do
          let name = case nameToken of TIdent n -> n; _ -> ""
          debugLog $ "Successfully consumed var name: " <> name

          -- Show next token before parsing equals
          nextTokens2 <- runParser peek tokens2
          case nextTokens2 of
            Right (mToken, _) -> debugLog $ "Next token before equals sign: " <> T.pack (show mToken)
            _ -> return ()

          -- Parse equals sign explicitly with detailed error handling
          eqResult <- runParser (consume (== TSymbol "=")) tokens2
          case eqResult of
            Left err -> do
              debugLog $ "FAILED to consume equals sign: " <> T.pack err
              -- Examine the tokens more closely
              let visibleTokens = take 5 $ dropWhile isLayout tokens2
              debugLog $ "Current tokens: " <> T.pack (show visibleTokens)
              -- Fail with more information
              return $ Left $ "Failed to parse equals sign: " ++ err ++ " Tokens: " ++ show visibleTokens
            Right (eqToken, tokens3) -> do
              debugLog $ "Successfully consumed equals sign: " <> T.pack (show eqToken)

              -- Show next token before parsing expression
              nextTokens3 <- runParser peek tokens3
              case nextTokens3 of
                Right (mToken, _) -> debugLog $ "Next token before expression: " <> T.pack (show mToken)
                _ -> return ()

              -- Parse value expression
              exprResult <- runParser parseExpr tokens3
              case exprResult of
                Left err -> return (Left $ "Failed to parse expression: " ++ err)
                Right (expr, tokens4) -> do
                  debugLog $ "Successfully parsed expression: " <> T.pack (show expr)

                  -- Return as a definition
                  debugLog "*** LEAVING parseBlockLet SUCCESSFULLY ***"
                  return (Right (Def name expr, tokens4))
  where
    isLayout t = case t of
      TIndent -> True
      TDedent -> True
      TNewline -> True
      _ -> False

-- Restore parseExprList for handling semicolon-separated expressions
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

-- Parse a single statement without top-level concerns
parseTopLevelStmt :: Parser ExprBodyExpr
parseTopLevelStmt = parseMacroDef <|> parseFnDef <|> parseExprStmt
  where
    parseExprStmt = do
      e <- parseExprList  -- Use parseExprList to handle semicolons
      return (Expr e)

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
      stmts <- parseTopStmt
      -- Skip any trailing newlines/empty lines
      _ <- skipEmptyLines
      -- Parse more statements
      rest <- parseMultipleTopStmts
      return (stmts ++ rest)
    Nothing -> return [] -- No more tokens

-- Update parseTopStmt to handle multiple statements from let
parseTopStmt :: Parser [ExprBodyExpr]
parseTopStmt =
  parseLet <|> -- Try to parse let first, returning multiple Def expressions
  (do
    stmt <- parseTopLevelStmt
    return [stmt]) -- Wrap single statements in a list

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

-- This function determines if the next tokens form an indented block
-- and parses accordingly
parseIndentedOrPlain :: Parser Expr
parseIndentedOrPlain = Parser $ \tokens -> do
  -- First, skip any immediate newlines
  let afterNewlines = dropWhile (== TNewline) tokens

  -- Check if the next token is an indent
  if not (null afterNewlines) && head afterNewlines == TIndent then do
    -- We found an indented block
    forceDebugLog "Found indented block - using parseBlock"
    runParser parseBlock tokens
  else do
    -- No indented block, parse a plain expression
    forceDebugLog "No indented block - parsing single expression"
    runParser parseExpr tokens


-- Expressions include if-expressions, addition, function calls, and primaries.
parseExpr :: Parser Expr
parseExpr = do
  nextTok <- peek
  case nextTok of
    Just (TKeyword "if") -> parseIf
    Just (TIdent "unless") -> parseMacroUsage
    -- No let case here, as let is handled specially in blocks
    _ -> parseComparison

-- Fix parseIf to handle indented blocks properly
parseIf :: Parser Expr
parseIf = do
  logInParser "Entering parseIf"
  _ <- consume (== TKeyword "if")

  -- Parse condition
  logInParser "Parsing if condition"
  cond <- parseComparison

  -- Expect colon
  _ <- consume (== TSymbol ":")
  logInParser "Consumed colon after condition"

  -- Check for newline specifically
  hasNewline <- checkForNewline
  thenExpr <- if hasNewline then do
    logInParser "Found newline after if condition"

    -- Consume the newline and indent
    _ <- consumeLayout (== TNewline)
    _ <- consumeLayout (== TIndent)

    -- Parse all statements until dedent
    stmts <- parseStatementsUntilDedent

    -- Process them into a proper block expression
    case stmts of
      [] -> do
        logInParser "Empty then-block, returning nil"
        return (ELit LNil)
      [Expr e] -> do
        logInParser "Single expression then-block"
        return e
      _ -> do
        let (defs, exprs) = partitionDefs stmts
        let finalExpr = if null exprs then ELit LNil else last exprs
        logInParser "Complex then-block with definitions"
        return $ EApp (ELam [] (ExprBody defs finalExpr)) []
  else do
    logInParser "No newline, parsing single expression for then branch"
    parseExpr

  -- Parse optional else clause
  mElse <- tryConsume (== TKeyword "else")
  case mElse of
    Just _ -> do
      logInParser "Found else clause"
      _ <- consume (== TSymbol ":")
      logInParser "Consumed colon after else"

      -- Check for newline after else:
      hasNewlineElse <- checkForNewline
      elseExpr <- if hasNewlineElse then do
        logInParser "Found newline after else:"

        -- Consume the newline and indent
        _ <- consumeLayout (== TNewline)
        _ <- consumeLayout (== TIndent)

        -- Parse all statements until dedent
        stmts <- parseStatementsUntilDedent

        -- Process them into a proper block expression
        case stmts of
          [] -> do
            logInParser "Empty else-block, returning nil"
            return (ELit LNil)
          [Expr e] -> do
            logInParser "Single expression else-block"
            return e
          _ -> do
            let (defs, exprs) = partitionDefs stmts
            let finalExpr = if null exprs then ELit LNil else last exprs
            logInParser "Complex else-block with definitions"
            return $ EApp (ELam [] (ExprBody defs finalExpr)) []
      else do
        logInParser "No newline, parsing single expression for else branch"
        parseExpr

      logInParser "Completed if-else expression"
      return (EIf cond thenExpr elseExpr)
    Nothing -> do
      logInParser "No else clause, using nil for else branch"
      return (EIf cond thenExpr (ELit LNil))

-- Helper to check for a NEWLINE token without consuming it
checkForNewline :: Parser Bool
checkForNewline = Parser $ \ts -> do
  -- Only look at the first token
  let result = case ts of
        (TNewline:_) -> True
        _ -> False
  forceDebugLog $ "checkForNewline result: " ++ show result ++ " from token: " ++
      (if null ts then "EMPTY" else show (head ts))
  return (Right (result, ts))

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

-- Parse comparisons (lowest precedence)
parseComparison :: Parser Expr
parseComparison = do
  lhs <- parseAddSub
  (do
    op <- tryConsume (\t -> case t of
           TSymbol s -> s `elem` ["<", ">", "<=", ">=", "=="]
           _ -> False)
    case op of
      Just tok -> do
        rhs <- parseAddSub
        let opName = case tok of
              TSymbol "<=" -> "<="
              TSymbol ">=" -> ">="
              TSymbol "<" -> "<"
              TSymbol ">" -> ">"
              TSymbol "==" -> "eq?"
              _ -> error "Unknown comparison operator"
        return $ EApp (EBuiltinIdent opName) [lhs, rhs]
      Nothing -> return lhs
   ) <|> return lhs

-- Helper to consume a comparison operator
consumeComparisonOp :: Parser Token
consumeComparisonOp =
  consume (\t -> case t of
              TSymbol s -> s `elem` ["<", ">", "<=", ">=", "=="]
              _ -> False)

-- Parse addition and subtraction (middle precedence)
parseAddSub :: Parser Expr
parseAddSub = do
  lhs <- parseMulDiv
  parseAddSubRest lhs
  where
    parseAddSubRest lhs =
      (do
        op <- tryConsume (\t -> case t of
               TSymbol "+" -> True
               TSymbol "-" -> True
               _ -> False)
        case op of
          Just (TSymbol "+") -> do
            rhs <- parseMulDiv
            let addExpr = EApp (EBuiltinIdent "+") [lhs, rhs]
            parseAddSubRest addExpr
          Just (TSymbol "-") -> do
            rhs <- parseMulDiv
            let subExpr = EApp (EBuiltinIdent "-") [lhs, rhs]
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
               TSymbol "*" -> True
               TSymbol "/" -> True
               _ -> False)
        case op of
          Just (TSymbol "*") -> do
            rhs <- parseUnary
            let mulExpr = EApp (EBuiltinIdent "*") [lhs, rhs]
            parseMulDivRest mulExpr
          Just (TSymbol "/") -> do
            rhs <- parseUnary
            let divExpr = EApp (EBuiltinIdent "/") [lhs, rhs]
            parseMulDivRest divExpr
          _ -> return lhs
      ) <|> return lhs

-- Parse unary operators like unary minus or not
parseUnary :: Parser Expr
parseUnary = do
  op <- tryConsume (\t -> case t of
         TSymbol "-" -> True
         TKeyword "not" -> True
         _ -> False)
  case op of
    Just (TSymbol "-") -> do
      expr <- parseUnary
      return $ EApp (EBuiltinIdent "-") [ELit (LInt 0), expr]  -- Implement unary minus as 0 - expr
    Just (TKeyword "not") -> do
      expr <- parseUnary
      return $ EApp (EBuiltinIdent "not") [expr]
    Nothing -> parseCall

-- Parse function application with either space or parentheses syntax
parseCall :: Parser Expr
parseCall = do
  primary <- parsePrimary
  parseCallRest primary
  where
    parseCallRest fun =
      -- Try parentheses syntax with multiple arguments
      ( do
          _ <- consume (== TSymbol "(")
          args <- parseArgList
          _ <- consume (== TSymbol ")")
          let callExpr = EApp fun args
          parseCallRest callExpr
      )
      <|>
      -- Try space-separated syntax for builtins
      ( case fun of
          EBuiltinIdent _ -> do
            arg <- parsePrimary -- Use parsePrimary to avoid left recursion
            let callExpr = EApp fun [arg]
            parseCallRest callExpr
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

-- Helper to parse a binding (name = expr)
parseBinding :: Parser (T.Text, Expr)
parseBinding = do
  logInParser "Entering parseBinding"
  nameToken <- consume (\t -> case t of TIdent _ -> True; _ -> False)
  let name = case nameToken of TIdent n -> n; _ -> ""
  logInParser $ "Parsed binding name: " ++ T.unpack name

  -- Peek at the next token before trying to consume equals
  mNextToken <- peek
  logInParser $ "Next token before equals: " ++ show mNextToken

  -- Try to consume equals sign
  equalToken <- consume (== TSymbol "=")
  logInParser $ "Consumed equals sign: " ++ show equalToken

  -- Parse the expression
  expr <- parseExpr
  logInParser "Parsed binding expression"

  return (name, expr)

-- Helper to parse a binding on a single line with no comma
parseBindingLine :: Parser (T.Text, Expr)
parseBindingLine = do
  binding <- parseBinding
  _ <- consumeLayout (== TNewline)
  return binding

-- Helper for parsing at least one occurrence of something
many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x : xs)
