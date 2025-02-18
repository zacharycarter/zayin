{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Zayin.Parser (parseProgram) where

import Control.Monad (void, when)
import Control.Monad.Identity (Identity)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (LanguageDef(..))
import qualified Text.Parsec.Token as Tok

import Zayin.AST
import Zayin.Literals

type Parser = ParsecT T.Text () Identity

-- Update langDef to use T.Text and include "print" in reservedNames
langDef :: Tok.GenLanguageDef T.Text () Identity
langDef = Tok.LanguageDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter <|> char '_'
  , Tok.identLetter     = alphaNum <|> char '_'
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = map T.unpack ["let", "in", "if", "then", "else", "print"]
  , Tok.reservedOpNames = map T.unpack ["+", "-", "*", "/", "="]
  , Tok.caseSensitive   = True
  }

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser langDef

reserved :: T.Text -> Parser ()
reserved = Tok.reserved lexer . T.unpack

reservedOp :: T.Text -> Parser ()
reservedOp = Tok.reservedOp lexer . T.unpack

identifier :: Parser T.Text
identifier = T.pack <$> Tok.identifier lexer

integer :: Parser Integer
integer = Tok.integer lexer

-- We keep parens, semiSep, whiteSpace as before.
parens :: Parser a -> Parser a
parens = Tok.parens lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

--------------------------------------------------------------------------------
-- Top-Level Parser
--------------------------------------------------------------------------------

parseProgram :: Parser Expr
parseProgram = whiteSpace *> parseExpr <* eof

parseExpr :: Parser Expr
parseExpr = buildExpressionParser operators parseTerm

operators :: [[Operator T.Text () Identity Expr]]
operators =
  [ [binary "*" AssocLeft, binary "/" AssocLeft, binary "%" AssocLeft]
  , [binary "+" AssocLeft, binary "-" AssocLeft]
  , [binary "==" AssocNone, binary "<" AssocNone, binary ">" AssocNone]
  ]
  where
    binary name assoc = Infix (do
      reservedOp name
      pure $ \x y -> EApp (EBuiltinIdent name) [x, y]) assoc

parseTerm :: Parser Expr
parseTerm = choice
  [ parseIf
  , parseLet
  , parseLambda
  , parseLiteral
  , parseBuiltin  -- see below
  , parseVar
  , parens parseExpr
  ]

parseLiteral :: Parser Expr
parseLiteral = ELit <$> choice
  [ LInt . fromInteger <$> integer
  , LString . T.pack <$> Tok.stringLiteral lexer
  , LNil <$ reserved "nil"
  ]

-- parseBuiltin now tries first to parse a builtin with an argument list
parseBuiltin :: Parser Expr
parseBuiltin = try parseBuiltinWithArg <|> parseBuiltinNoArg
  where
    parseBuiltinNoArg = choice
      [ EBuiltinIdent <$> (reserved "isInt"  >> return "isInt")
      , EBuiltinIdent <$> (reserved "isZero" >> return "isZero")
      , EBuiltinIdent <$> (reserved "isNil"  >> return "isNil")
      , EBuiltinIdent <$> (reserved "isBool" >> return "isBool")
      , EBuiltinIdent <$> (reserved "print"  >> return "display")
      ]
    parseBuiltinWithArg = do
      reserved "print"
      args <- parens (sepBy parseExpr (reservedOp ","))
      return $ EApp (EBuiltinIdent "display") args

parseVar :: Parser Expr
parseVar = EVar <$> identifier

parseIf :: Parser Expr
parseIf = do
  reserved "if"
  cond <- parseExpr
  reserved "then"
  thenExpr <- parseExpr
  reserved "else"
  elseExpr <- parseExpr
  return $ EIf cond thenExpr elseExpr

parseLet :: Parser Expr
parseLet = do
  reserved "let"
  (bindings, body) <- try parseLetInline <|> parseLetBlock
  return $ ELet bindings body

parseLetInline :: Parser ([(T.Text, Expr)], ExprBody)
parseLetInline = do
  binding <- parseBinding
  reservedOp ";"
  body <- parseExprBody
  return ([binding], body)

parseLetBlock :: Parser ([(T.Text, Expr)], ExprBody)
parseLetBlock = do
  (pos, _) <- getPosition >>= \p -> (p, p) <$ many newline
  bindings <- indented pos $ many1 (parseBinding <* optional (reservedOp ";"))
  body <- parseExprBody
  return (bindings, body)
  where
    indented pos p = do
      newline
      col <- sourceColumn <$> getPosition
      when (col <= sourceColumn pos) $
        fail "insufficient indentation"
      p

parseBinding :: Parser (T.Text, Expr)
parseBinding = do
  name <- identifier
  reservedOp "="
  expr <- parseExpr
  return (name, expr)

parseLambda :: Parser Expr
parseLambda = do
  reserved "fn"
  params <- parens (many identifier) <|> pure []
  reservedOp "="
  body <- parseExprBody
  return $ ELam params body

parseExprBody :: Parser ExprBody
parseExprBody = do
  entries <- many (try parseDef <|> (Expr <$> parseExpr) <* optional (reservedOp ";"))
  final <- parseExpr
  return $ ExprBody entries final

parseDef :: Parser ExprBodyExpr
parseDef = do
  reserved "let"
  name <- identifier
  reservedOp "="
  expr <- parseExpr
  return $ Def name expr
