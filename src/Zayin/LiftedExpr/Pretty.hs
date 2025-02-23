{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Zayin.LiftedExpr.Pretty (renderLExpr, renderLiftedLambda) where

-- Import Pretty instance for Literal

import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Zayin.AST.Pretty ()
import Zayin.LiftedExpr (LExpr (..), LiftedLambda (..))
import Zayin.Literals (Literal)

-- | Pretty printing for our LExpr type.
instance Pretty LExpr where
  pretty = \case
    Var v ->
      pretty v
    Lit l ->
      pretty l -- Uses the Literal Pretty instance from AST.Pretty
    BuiltinIdent i ->
      pretty i
    SetThen var val cont ->
      parens (group ("set-then!" <+> pretty var <+> pretty val <+> pretty cont))
    If cond t f ->
      parens (group ("if" <+> pretty cond <+> pretty t <+> pretty f))
    Lifted lambdaId ->
      "lifted-lambda@" <> pretty lambdaId
    CallOne f arg ->
      parens (group (pretty f <+> pretty arg))
    CallTwo f arg1 arg2 ->
      parens (group (pretty f <+> pretty arg1 <+> pretty arg2))

-- | Pretty printing for LiftedLambda
instance Pretty LiftedLambda where
  pretty lambda =
    vsep
      [ "LiftedLambda" <+> pretty (lambdaId lambda),
        indent 2 $
          vsep
            [ "params:" <+> hsep (map pretty (params lambda)),
              "free:" <+> hsep (map pretty (toList (freeVars lambda))),
              "body:" <+> pretty (body lambda)
            ]
      ]

-- | A helper to render an LExpr as a String.
renderLExpr :: LExpr -> String
renderLExpr = renderString . layoutPretty defaultLayoutOptions . pretty

-- | A helper to render a LiftedLambda as a String.
renderLiftedLambda :: LiftedLambda -> String
renderLiftedLambda = renderString . layoutPretty defaultLayoutOptions . pretty
