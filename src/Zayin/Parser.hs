{-# LANGUAGE OverloadedStrings #-}

module Zayin.Parser
  ( parseProgram
  , Expr(..)
  , ExprBodyExpr(..)
  , ExprBody(..)
  , Literal(..)
  ) where

import Control.Monad (void)
import Control.Monad.Logger (MonadLogger, logDebugN)
import Control.Monad.Trans (lift)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
  ( ParsecT
  , (<|>)
  , between
  , many
  , some
  , manyTill
  , notFollowedBy
  , try
  , empty
  , eof
  , runParserT
  , sepBy
  , sepEndBy1
  , lookAhead
  , anySingle
  , ParseErrorBundle
  )
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as L

import Zayin.AST
import Zayin.Literals

-- | Our parser type. We use ParsecT over an arbitrary monad m that is an instance of MonadLogger.
type Parser m = ParsecT Void Text m

------------------------------------------------------------
-- Lexing helpers
------------------------------------------------------------

-- | Consume whitespace and comments.
sc :: (Monad m) => Parser m ()
sc = L.space MC.space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment ";"      -- Rebol uses ';' for comments
    blockCmnt = L.skipBlockComment "/*" "*/" -- Alternative style

-- | Parse a lexeme and consume trailing space.
lexeme :: (Monad m) => Parser m a -> Parser m a
lexeme = L.lexeme sc

-- | Parse a symbol.
symbol :: (Monad m) => Text -> Parser m Text
symbol = L.symbol sc

-- | Parse a reserved keyword.
reserved :: (Monad m) => Text -> Parser m ()
reserved w = void $ lexeme $ MC.string w

------------------------------------------------------------
-- Tokens
------------------------------------------------------------

-- | Recognized operators
operators :: [Text]
operators = ["+", "-", "*", "/", "%", "<", ">", "<=", ">=", "=", "==", "!=", "^", "and", "or"]

-- | Recognized builtin functions
builtins :: [Text]
builtins = [
    "display", "print", "exit", "tostring",
    "cons", "car", "cdr", "cons?", "null?",
    "string-concat", "string-chars"
  ]

-- | Reserved words
reservedWords :: [Text]
reservedWords = ["func", "either", "context", "true", "false", "none"]

-- | Parse a number literal
number :: (Monad m) => Parser m Literal
number = lexeme $ do
  try parseFloat <|> parseInt
  where
    parseInt = L.signed sc L.decimal >>= \n -> return (LInt n)
    parseFloat = L.signed sc L.float >>= \f -> return (LFloat f)

-- | Parse a string literal
string :: (Monad m) => Parser m Literal
string = lexeme $ do
  _ <- MC.char '"'
  str <- manyTill L.charLiteral (MC.char '"')
  return $ LString (T.pack str)

-- | Parse a boolean literal
boolean :: (Monad m) => Parser m Literal
boolean = lexeme $ do
  b <- try (MC.string "true") <|> try (MC.string "false")
  return $ LBool (b == "true")

-- | Parse a nil literal
nil :: (Monad m) => Parser m Literal
nil = lexeme $ MC.string "none" >> return LNil

-- | Parse an identifier, checking if it's a keyword
identifier :: (Monad m) => Parser m Text
identifier = lexeme $ do
  first <- MC.letterChar <|> MC.char '_'
  rest <- many (MC.alphaNumChar <|> MC.char '_' <|> MC.char '-' <|> MC.char '?' <|> MC.char '!')
  let ident = T.pack (first:rest)
  if ident `elem` reservedWords
    then fail $ "Reserved word: " ++ T.unpack ident
    else return ident

------------------------------------------------------------
-- Top-level program parser
------------------------------------------------------------

-- Parse an entire program using the Rebol-like approach
parseProgram :: (MonadLogger m) => Text -> m (Either (ParseErrorBundle Text Void) Expr)
parseProgram input = runParserT (sc >> program) "<program>" input

-- A program is a sequence of definitions and expressions,
-- implicitly wrapped in a lambda function that is immediately applied
program :: (MonadLogger m) => Parser m Expr
program = do
  lift $ logDebugN "Parsing top-level program..."
  sc

  -- Parse all lines as separate statements
  stmts <- sepEndBy1 statement (some (MC.newline <|> MC.char ';'))

  -- Create the program structure - lambda with body
  let defs = [def | def@(Def _ _) <- stmts]
      exprs = [e | Expr e <- stmts]
      finalExpr = if null exprs then ELit LNil else last exprs
      body = ExprBody defs finalExpr

  return $ EApp (ELam [] body) []

-- Parse a single statement (definition or expression)
statement :: (MonadLogger m) => Parser m ExprBodyExpr
statement = do
  lift $ logDebugN "Parsing statement..."
  -- First try to parse a definition, then an expression
  try definition <|> (Expr <$> expr)

-- Parse a definition: an identifier followed by colon and expression
definition :: (MonadLogger m) => Parser m ExprBodyExpr
definition = do
  lift $ logDebugN "Parsing definition..."
  name <- identifier
  void $ symbol ":"
  value <- expr
  return $ Def name value

-- Parse an expression using Rebol's approach
expr :: (MonadLogger m) => Parser m Expr
expr = do
  lift $ logDebugN "Parsing expression..."

  -- Parse a left-associative infix expression
  chainl1 term operator

-- Chain left-associative operations
chainl1 :: (MonadLogger m) => Parser m Expr -> Parser m (Expr -> Expr -> Expr) -> Parser m Expr
chainl1 p op = do
  x <- p
  rest x
  where
    rest x = do
      f <- op
      y <- p
      rest (f x y)
      <|> return x

-- Parse an operator with precedence handling
operator :: (MonadLogger m) => Parser m (Expr -> Expr -> Expr)
operator = do
  op <- lexeme $ choice (map MC.string operators)
  return $ \x y -> EApp (EBuiltinIdent op) [x, y]
  where
    choice = foldr (<|>) empty

-- Parse a term: literal, variable, function call, or special form
term :: (MonadLogger m) => Parser m Expr
term = try functionCall <|> primary

-- Parse a primary expression
primary :: (MonadLogger m) => Parser m Expr
primary = do
  try ifExpr <|>
    try funcExpr <|>
    try contextExpr <|>
    try literalExpr <|>
    try identExpr <|>
    parens expr

-- Parse a literal expression
literalExpr :: (Monad m) => Parser m Expr
literalExpr = ELit <$> (try string <|> try number <|> try boolean <|> nil)

-- Parse an identifier: either variable or builtin
identExpr :: (Monad m) => Parser m Expr
identExpr = do
  name <- identifier
  return $ if name `elem` builtins
    then EBuiltinIdent name
    else EVar name

-- Parse a function call: function name followed by argument
functionCall :: (MonadLogger m) => Parser m Expr
functionCall = do
  -- Parse the function name
  func <- try $ do
    f <- identExpr
    -- Must be followed by space and then another expression
    sc
    return f

  -- Parse the argument
  arg <- expr

  return $ EApp func [arg]

-- | Parse content in parentheses.
parens :: (Monad m) => Parser m a -> Parser m a
parens = between (symbol "(") (symbol ")")

-- | Parse content in square brackets.
brackets :: (Monad m) => Parser m a -> Parser m a
brackets = between (symbol "[") (symbol "]")

------------------------------------------------------------
-- Special forms
------------------------------------------------------------

-- | Parse an if-expression (using the "either" keyword).
-- Syntax: either <cond> [ <then> ] [ <else> ]
ifExpr :: (MonadLogger m) => Parser m Expr
ifExpr = do
  lift $ logDebugN "Parsing if-expression..."
  reserved "either"
  cond <- expr
  thenExpr <- brackets expr
  elseExpr <- brackets expr
  return $ EIf cond thenExpr elseExpr

-- | Parse a lambda expression.
-- Syntax: func [<params>] [<body>]
funcExpr :: (MonadLogger m) => Parser m Expr
funcExpr = do
  lift $ logDebugN "Parsing lambda expression..."
  reserved "func"
  params <- brackets (identifier `sepBy` sc)
  bodyBlock <- parseBody
  return $ ELam params bodyBlock

-- | Parse a let-binding expression.
-- Syntax: context [<bindings>] [<body>]
contextExpr :: (MonadLogger m) => Parser m Expr
contextExpr = do
  lift $ logDebugN "Parsing let-binding expression..."
  reserved "context"
  bindings <- brackets (many definition)
  bodyBlock <- parseBody
  return $ ELet [ (name, e) | Def name e <- bindings ] bodyBlock

-- Parse a body block (used in special forms)
parseBody :: (MonadLogger m) => Parser m ExprBody
parseBody = brackets $ do
  -- Parse all statements (definitions and expressions)
  statements <- many $ try definition <|> Expr <$> expr

  -- Extract definitions and expressions
  let defs = [def | def@(Def _ _) <- statements]
      exprs = [e | Expr e <- statements]
      finalExpr = if null exprs then ELit LNil else last exprs

  return $ ExprBody defs finalExpr
