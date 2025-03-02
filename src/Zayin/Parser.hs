{-# LANGUAGE OverloadedStrings #-}
module Zayin.Parser
  ( parseProgram  -- :: Text -> Either (ParseErrorBundle Text Void) Expr
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
import Data.Functor.Identity (Identity)
import Text.Megaparsec
  ( ParsecT
  , (<|>)
  , between
  , many
  , manyTill
  , notFollowedBy
  , try
  , empty
  , eof
  , runParserT
  , sepBy
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
    lineCmnt  = L.skipLineComment ";"      -- Rebol often uses ';' for comments.
    blockCmnt = L.skipBlockComment "/*" "*/" -- if needed

-- | Parse a lexeme and consume trailing space.
lexeme :: (Monad m) => Parser m a -> Parser m a
lexeme = L.lexeme sc

-- | Parse a symbol.
symbol :: (Monad m) => Text -> Parser m Text
symbol = L.symbol sc

-- | Parse a reserved keyword and make sure it is not used as an identifier.
reserved :: (Monad m) => Text -> Parser m ()
reserved w = void (lexeme (MC.string w))

------------------------------------------------------------
-- Identifier and literal parsers
------------------------------------------------------------

-- | An identifier starts with a letter or underscore and then alphanumerics,
-- underscores or hyphens. We disallow reserved words.
identifier :: (Monad m) => Parser m Text
identifier = lexeme $ do
  first <- MC.letterChar <|> MC.char '_'
  rest  <- many (MC.alphaNumChar <|> MC.char '_' <|> MC.char '-')
  let ident = T.pack (first:rest)
  if ident `elem` ["func", "either", "block"]
     then fail $ "Reserved word: " ++ T.unpack ident
     else return ident

-- | Parse a builtin operator symbol.
builtinOp :: (Monad m) => Parser m Text
builtinOp = lexeme $ choice (map MC.string ["+", "-", "*", "/", "<=", ">=", "<", ">", "="])
  where
    choice = foldr (<|>) empty

-- | Parse a literal.
literal :: (Monad m) => Parser m Literal
literal = try parseBool <|> try parseNil <|> try parseFloat <|> try parseInt <|> parseString
  where
    parseString = do
      _ <- MC.char '"'
      str <- manyTill L.charLiteral (MC.char '"')
      return $ LString (T.pack str)
    parseInt = L.decimal >>= \n -> return (LInt n)
    parseFloat = L.float >>= \f -> return (LFloat f)
    parseBool = (MC.string "true" >> return (LBool True))
            <|> (MC.string "false" >> return (LBool False))
    parseNil = MC.string "nil" >> return LNil

------------------------------------------------------------
-- Bracketing helpers
------------------------------------------------------------

-- | Parse content in parentheses.
parens :: (Monad m) => Parser m a -> Parser m a
parens = between (symbol "(") (symbol ")")

-- | Parse content in square brackets.
brackets :: (Monad m) => Parser m a -> Parser m a
brackets = between (symbol "[") (symbol "]")

------------------------------------------------------------
-- Top-level program parser
------------------------------------------------------------

-- | Parse an entire program. A program is a series of definitions and expressions
-- that is automatically wrapped in an anonymous lambda (with no parameters) and then applied.
parseProgram :: (MonadLogger m) => Text -> m (Either (ParseErrorBundle Text Void) Expr)
parseProgram input = runParserT (sc >> program <* eof) "<program>" input

program :: (MonadLogger m) => Parser m Expr
program = do
  lift $ logDebugN "Parsing top-level program..."
  -- Parse zero or more statements (definitions or expressions)
  stmts <- many stmt
  -- The final expression of the program.
  final <- expr
  let body = ExprBody { bodyExprs = stmts, finalExpr = final }
  -- Wrap in an anonymous lambda that is immediately applied.
  return $ EApp (ELam [] body) []

------------------------------------------------------------
-- Statement and expression bodies
------------------------------------------------------------

-- | A statement is either a definition or an expression.
stmt :: (MonadLogger m) => Parser m ExprBodyExpr
stmt = try definition <|> (Expr <$> expr)

-- | A definition: an identifier followed by ':' and an expression.
definition :: (MonadLogger m) => Parser m ExprBodyExpr
definition = do
  lift $ logDebugN "Parsing definition..."
  name <- identifier
  _ <- symbol ":"
  e <- expr
  return $ Def name e

-- | Parse an expression body (used for lambda and let bodies).
exprBody :: (MonadLogger m) => Parser m ExprBody
exprBody = do
  lift $ logDebugN "Parsing expression body..."
  stmts <- many stmt
  final <- expr
  return $ ExprBody stmts final

------------------------------------------------------------
-- Expression parser
------------------------------------------------------------

-- The top-level expression parser first parses a “term” (which includes infix operators)
-- and then (if there are subsequent terms that are not infix-bound) treats those as arguments
-- to a function call. This gives infix operators higher precedence than function application.
expr :: (MonadLogger m) => Parser m Expr
expr = do
  lift $ logDebugN "Parsing expression..."
  t <- term 
  args <- many term
  return $ if null args then t else EApp t args

-- | Parse a term. A term is a primary expression chained (left-associatively)
-- with infix operators. This ensures that in an expression like “factorial n - 1”
-- the infix “-” binds to “n” and “1” before being passed as an argument to “factorial”.
term :: (MonadLogger m) => Parser m Expr
term = chainl1 primary opParser

-- | Parse an infix operator and return a function to combine two expressions.
opParser :: (Monad m) => Parser m (Expr -> Expr -> Expr)
opParser = do
  opSym <- builtinOp
  return (\l r -> EApp (EBuiltinIdent opSym) [l, r])

-- | A standard left-associative chain parser.
chainl1 :: (Monad m) => Parser m a -> Parser m (a -> a -> a) -> Parser m a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x = (do
                f <- op
                y <- p
                rest (f x y))
             <|> return x

------------------------------------------------------------
-- Primary expressions
------------------------------------------------------------

-- | A primary expression is one of several atomic constructs.
primary :: (MonadLogger m) => Parser m Expr
primary = choice
  [ try ifExpr
  , try lamExpr
  , try letExpr
  , ELit <$> literal
  , EVar <$> identifier
  , parens expr
  ]
  where
    choice = foldr (<|>) empty

------------------------------------------------------------
-- Special forms
------------------------------------------------------------

-- | Parse an if-expression (using the “either” keyword).
-- Syntax: either <cond> [ <then> ] [ <else> ]
ifExpr :: (MonadLogger m) => Parser m Expr
ifExpr = do
  lift $ logDebugN "Parsing if-expression..."
  _ <- MC.string "either" >> sc
  cond <- expr
  thenExpr <- brackets expr
  elseExpr <- brackets expr
  return $ EIf cond thenExpr elseExpr

-- | Parse a lambda expression.
-- Syntax: func [ <params> ] [ <body> ]
lamExpr :: (MonadLogger m) => Parser m Expr
lamExpr = do
  lift $ logDebugN "Parsing lambda expression..."
  _ <- MC.string "func" >> sc
  params <- brackets (identifier `sepBy` sc)
  body <- brackets exprBody
  return $ ELam params body

-- | Parse a let-binding expression.
-- Syntax: block [ <definitions> ] [ <body> ]
letExpr :: (MonadLogger m) => Parser m Expr
letExpr = do
  lift $ logDebugN "Parsing let-binding expression..."
  _ <- MC.string "block" >> sc
  defs <- brackets (many definition)
  body <- brackets exprBody
  return $ ELet [ (name, e) | Def name e <- defs ] body
