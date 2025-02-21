{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Zayin.FlatExpr.Pretty (renderFExpr) where

import Zayin.FlatExpr (FExpr(..))
import Zayin.Literals (Literal)
import Zayin.AST.Pretty ()  -- Import Pretty instance for Literal
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import qualified Prettyprinter.Render.Terminal as ANSI

-- | Pretty printing for our FExpr type.
instance Pretty FExpr where
  pretty = \case
    If cond t f ->
      parens ("if" <+> pretty cond <+> pretty t <+> pretty f)

    LamOne param body ->
      parens ("lambda" <+> parens (pretty param) <> line <> pretty body)

    LamTwo param1 param2 body ->
      parens ("lambda" <+> parens (pretty param1 <+> pretty param2) <> line <> pretty body)

    Var v ->
      pretty v

    Lit l ->
      pretty l  -- Uses the Literal Pretty instance from AST.Pretty

    BuiltinIdent i ->
      pretty i

    SetThen var val cont ->
      parens ("set-then!" <+> pretty var <+> pretty val <+> pretty cont)

    CallOne f arg ->
      parens (pretty f <+> pretty arg)

    CallTwo f arg1 arg2 ->
      parens (pretty f <+> pretty arg1 <+> pretty arg2)

-- | A helper to render an FExpr as a String.
renderFExpr :: FExpr -> String
renderFExpr = renderString . layoutPretty defaultLayoutOptions . pretty

-- -- | A helper to render an FExpr with ANSI colors
-- renderFExprColored :: FExpr -> String
-- renderFExprColored =
--   ANSI.renderStrict . layoutPretty defaultLayoutOptions . unAnnotateS . pretty
