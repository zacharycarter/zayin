{-# LANGUAGE OverloadedStrings #-}

module Zayin.AST.Pretty (renderExpr) where

import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Zayin.AST
import Zayin.Literals (Literal (..))

-- | Pretty printing instance for our Literal.
instance Pretty Literal where
  pretty lit = case lit of
    LInt i -> pretty i
    LString s -> dquotes (pretty s)
    LBool b -> pretty b
    LNil -> "nil"

-- extend as needed for other literal types

-- | Pretty printing for our Expr type.
instance Pretty Expr where
  pretty expr = case expr of
    EVar s ->
      pretty s
    ELit lit ->
      pretty lit
    EBuiltinIdent s ->
      pretty s
    EIf cond t f ->
      parens ("if" <+> pretty cond <+> pretty t <+> pretty f)
    ESet name e ->
      parens ("set!" <+> pretty name <+> pretty e)
    ELet bindings body ->
      parens
        ( "let"
            <+> parens (hsep (map prettyBinding bindings))
            <> line
            <> pretty body
        )
      where
        prettyBinding (n, e) = parens (pretty n <+> pretty e)
    ELam params body ->
      parens
        ( "lambda"
            <+> parens (hsep (map pretty params))
            <> line
            <> pretty body
        )
    EApp f args ->
      parens (pretty f <+> hsep (map pretty args))

-- | Pretty printing for an expression body.
instance Pretty ExprBody where
  pretty (ExprBody exprs final) =
    vcat (map pretty exprs) <> line <> pretty final

-- | Pretty printing for expressions inside an expression body.
instance Pretty ExprBodyExpr where
  pretty (Def name e) =
    parens ("define" <+> pretty name <+> pretty e)
  pretty (Expr e) = pretty e

-- | A helper to render an Expr as a String.
renderExpr :: Expr -> String
renderExpr = renderString . layoutPretty defaultLayoutOptions . pretty
