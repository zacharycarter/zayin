{-# LANGUAGE OverloadedStrings #-}

module Zayin.BoundExpr.Pretty (renderBExpr) where

import Data.Text (Text)
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Zayin.BoundExpr
import Zayin.Literals (Literal (..))

instance Pretty Literal where
  pretty lit = case lit of
    LInt i -> pretty i
    LString s -> dquotes (pretty s)
    LBool b -> pretty b
    LNil -> "nil"

instance Pretty BExpr where
  pretty expr = case expr of
    Var s -> pretty s
    Lit lit -> pretty lit
    BuiltinIdent s -> pretty s
    Set name e ->
      parens $ "set!" <+> pretty name <+> pretty e
    Lam param body ->
      parens $ "lambda" <+> parens (pretty param) <+> pretty body
    If c t f ->
      parens $ "if" <+> pretty c <+> pretty t <+> pretty f
    App f a ->
      parens $ pretty f <+> pretty a

-- | Render a BExpr as a String.
renderBExpr :: BExpr -> String
renderBExpr = renderString . layoutPretty defaultLayoutOptions . pretty
