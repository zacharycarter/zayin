module Zayin.BoundExpr (BExpr (..)) where

import qualified Data.Text as T
import Zayin.Literals (Literal)

data BExpr
  = Var T.Text
  | Lit Literal
  | BuiltinIdent T.Text
  | If BExpr BExpr BExpr
  | Set T.Text BExpr
  | Lam T.Text BExpr
  | App BExpr BExpr
  deriving (Show, Eq)
