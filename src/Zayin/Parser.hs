{-# LANGUAGE OverloadedStrings #-}

module Zayin.Parser (
  parseWithDebug
) where

import Control.Monad (unless, when)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import qualified Data.Text as T

import Zayin.AST
import Zayin.Lexer
import Zayin.Literals

-- Parser state
data ParserState = ParserState
  { tokens :: [Token]
  , debugMode :: Bool
  }

-- Parser monad
type Parser a = ExceptT String (StateT ParserState IO) a

-- Run the parser
runParser :: Parser a -> ParserState -> IO (Either String a)
runParser parser state = do
  (result, _) <- runStateT (runExceptT parser) state
  return result

-- Log debug info
logDebugN :: T.Text -> Parser ()
logDebugN msg = do
  debug <- gets debugMode
  when debug $ liftIO $ putStrLn $ T.unpack msg

-- Check if text matches builtin name
isBuiltin :: T.Text -> Bool
isBuiltin w = w `elem` [T.pack "display", T.pack "not", T.pack "gc_collect", T.pack "gc_stats", T.pack "spawn", T.pack "cons",
                        T.pack "+", T.pack "-", T.pack "*", T.pack "/", T.pack "%", T.pack "<", T.pack ">",
                        T.pack "<=", T.pack ">=", T.pack "eq?", T.pack "car", T.pack "cdr"]

-- Get the next token without consuming it
peekToken :: Parser (Maybe Token)
peekToken = do
  toks <- gets tokens
  return $ case toks of
    [] -> Nothing
    (t:_) -> Just t

-- Consume the next token
nextToken :: Parser Token
nextToken = do
  toks <- gets tokens
  case toks of
    [] -> throwError "Unexpected end of input"
    (t:ts) -> do
      modify $ \s -> s { tokens = ts }
      return t

-- Consume a token if it matches a predicate
satisfy :: (Token -> Bool) -> String -> Parser Token
satisfy pred err = do
  t <- nextToken
  if pred t
    then return t
    else throwError $ err ++ ": " ++ show t

-- Match a specific token
match :: Token -> Parser ()
match expected = do
  t <- nextToken
  unless (t == expected) $
    throwError $ "Expected " ++ show expected ++ ", got " ++ show t

-- Parse a literal value
parseLiteral :: Parser Expr
parseLiteral = do
  tok <- nextToken
  case tok of
    TString s -> return $ ELit (LString s)
    TInteger n -> return $ ELit (LInt n)
    TFloat f -> return $ ELit (LFloat f)
    TBool b -> return $ ELit (LBool b)
    TNil -> return $ ELit LNil
    _ -> throwError $ "Expected literal, got " ++ show tok

-- Parse a variable reference
parseVar :: Parser Expr
parseVar = do
  tok <- nextToken
  case tok of
    TWord name -> return $ EVar name
    _ -> throwError $ "Expected variable name, got " ++ show tok

-- Parse a block of expressions
parseBlock :: Parser ExprBody
parseBlock = do
  match TLeftBracket
  exprs <- parseBodyExprs []
  match TRightBracket
  case exprs of
    [] -> throwError "Empty block not allowed"
    _ -> do
      let bodyExps = init exprs
      let finalExp = last exprs
      case finalExp of
        Expr e -> return $ ExprBody bodyExps e
        Def name e -> do
          -- If the last expression is a definition, we consider it part of bodyExprs
          -- with an implicit nil as the final expression
          return $ ExprBody (exprs) (ELit LNil)

-- Parse a series of body expressions
parseBodyExprs :: [ExprBodyExpr] -> Parser [ExprBodyExpr]
parseBodyExprs acc = do
  maybeToken <- peekToken
  case maybeToken of
    Nothing -> return $ reverse acc  -- End of input - return accumulated expressions
    Just TRightBracket -> return $ reverse acc
    Just (TWord name) -> do
      nextToken
      maybeColon <- peekToken
      case maybeColon of
        Just TColon -> do
          match TColon
          expr <- parseExpr
          parseBodyExprs (Def name expr : acc)
        _ -> do
          -- Put the token back
          modify $ \s -> s { tokens = TWord name : tokens s }
          expr <- parseExpr
          parseBodyExprs (Expr expr : acc)
    _ -> do
      expr <- parseExpr
      parseBodyExprs (Expr expr : acc)

-- Parse a set expression (assignment)
parseSet :: Parser Expr
parseSet = do
  name <- nextToken
  case name of
    TWord varName -> do
      match TColon
      expr <- parseExpr
      return $ ESet varName expr
    _ -> throwError $ "Expected variable name, got " ++ show name

-- Parse an if expression
parseIf :: Parser Expr
parseIf = do
  match TEither
  cond <- parseExpr
  thenBlock <- parseBlock
  elseBlock <- parseBlock
  return $ EIf cond (finalExpr thenBlock) (finalExpr elseBlock)

-- Parse a lambda expression
parseLambda :: Parser Expr
parseLambda = do
  match TFunc
  match TLeftBracket
  params <- parseParams []
  match TRightBracket
  body <- parseBlock
  return $ ELam params body

-- Parse lambda parameters
parseParams :: [T.Text] -> Parser [T.Text]
parseParams acc = do
  maybeToken <- peekToken
  case maybeToken of
    Just TRightBracket -> return $ reverse acc
    Just (TWord name) -> do
      nextToken
      parseParams (name : acc)
    Just tok -> throwError $ "Expected parameter name, got " ++ show tok
    Nothing -> throwError "Unexpected end of input while parsing parameters"

-- Parse a let expression
parseLet :: Parser Expr
parseLet = do
  match TContext
  match TLeftBracket
  bindings <- parseBindings []
  match TRightBracket
  body <- parseBlock
  return $ ELet bindings body

-- Parse let bindings
parseBindings :: [(T.Text, Expr)] -> Parser [(T.Text, Expr)]
parseBindings acc = do
  maybeToken <- peekToken
  case maybeToken of
    Just TRightBracket -> return $ reverse acc
    Just (TWord name) -> do
      nextToken
      match TColon
      expr <- parseExpr
      parseBindings ((name, expr) : acc)
    Just tok -> throwError $ "Expected binding name, got " ++ show tok
    Nothing -> throwError "Unexpected end of input while parsing bindings"

-- Parse a function application
parseApplication :: Expr -> Parser Expr
parseApplication func = do
  args <- parseArgs []
  return $ EApp func args

-- Parse function arguments
parseArgs :: [Expr] -> Parser [Expr]
parseArgs acc = do
  maybeToken <- peekToken
  case maybeToken of
    Just TRightBracket -> return $ reverse acc
    Just TRightBrace -> return $ reverse acc
    Just TRightParen -> return $ reverse acc
    Nothing -> return $ reverse acc
    Just token -> do
      -- Determine if this is the end of arguments
      isEndOfArgs <- case token of
        TWord _ -> do
          -- Check if next token is a colon (indicating var: expr)
          toks <- gets tokens
          return $ case drop 1 toks of
            (TColon:_) -> True
            _ -> False
        TEither -> return True
        TFunc -> return True
        TContext -> return True
        _ -> return False

      if isEndOfArgs
        then return $ reverse acc
        else do
          arg <- parseExpr
          parseArgs (arg : acc)

-- Parse an expression
parseExpr :: Parser Expr
parseExpr = do
  maybeToken <- peekToken
  case maybeToken of
    Just (TWord name) -> do
      nextToken
      -- Look ahead to see if any arguments follow
      maybeNextToken <- peekToken
      hasArgs <- checkIfHasArguments maybeNextToken
      if hasArgs
        then do
          -- This is a function application
          args <- parseArgs []
          return $ if (isBuiltin name) then EApp (EBuiltinIdent name) args else EApp (EVar name) args
        else
          -- This is just a variable reference
          return $ EVar name
    Just (TString _) -> parseLiteral
    Just (TInteger _) -> parseLiteral
    Just (TFloat _) -> parseLiteral
    Just (TBool _) -> parseLiteral
    Just TNil -> parseLiteral
    Just TEither -> parseIf
    Just TFunc -> parseLambda
    Just TContext -> parseLet
    Just TLeftBracket -> do
      block <- parseBlock
      return $ finalExpr block
    Just tok -> throwError $ "Unexpected token in expression: " ++ show tok
    Nothing -> throwError "Unexpected end of input while parsing expression"

-- Helper to check if arguments follow
checkIfHasArguments :: Maybe Token -> Parser Bool
checkIfHasArguments Nothing = return False
checkIfHasArguments (Just token) = case token of
  TRightBracket -> return False
  TRightBrace -> return False
  TRightParen -> return False
  TColon -> return False
  -- Keywords that would start a new expression
  TEither -> return False
  TFunc -> return False
  TContext -> return False
  TWord name | (isBuiltin name) -> return True
  -- Any other token suggests an argument
  _ -> return True

-- Parse a program (the entry point)
parseProgram :: Parser Expr
parseProgram = do
  logDebugN "Starting to parse program"
  -- We wrap the program in a lambda to ensure it's a complete expression
  exprs <- parseBodyExprs []
  if null exprs
    then return $ EApp (ELam [] (ExprBody [] (ELit LNil))) []
    else do
      -- If all expressions are definitions (no final expression),
      -- add an implicit nil as the final expression
      let allDefs = all isDef exprs
      if allDefs
        then do
          let body = ExprBody exprs (ELit LNil)
          return $ EApp (ELam [] body) []
        else do
          let bodyExps = init exprs
          let finalExp = last exprs
          case finalExp of
            Expr e -> do
              let body = ExprBody bodyExps e
              return $ EApp (ELam [] body) []
            Def name e -> do
              -- If the last expression is a definition, we consider it part of bodyExprs
              -- with an implicit nil as the final expression
              let body = ExprBody exprs (ELit LNil)
              return $ EApp (ELam [] body) []
  where
    isDef (Def _ _) = True
    isDef _ = False

-- Main parse function that's exposed
parseWithDebug :: Bool -> T.Text -> IO (Either String Expr)
parseWithDebug debug src = do
  tokensEither <- return $ tokenize src
  case tokensEither of
    Left err -> return $ Left $ "Tokenization error: " ++ err
    Right toks -> do
      when debug $ putStrLn $ "Tokens: " ++ show toks
      runParser parseProgram (ParserState toks debug)
