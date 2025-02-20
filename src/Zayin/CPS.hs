{-# LANGUAGE OverloadedStrings #-}

module Zayin.CPS (AExp (..), CExp (..)) where

import qualified Data.Text as T
import Zayin.Literals (Literal)

data AExp
  = Lam2 T.Text T.Text CExp
  | Lam1 T.Text CExp
  | Var T.Text
  | BuiltinIdent T.Text
  | Lit Literal
  deriving (Show, Eq)

data CExp
  = If AExp CExp CExp
  | SetThen T.Text AExp CExp
  | Call1 AExp AExp
  | Call2 AExp AExp AExp
  | DupThen T.Text AExp CExp
  | DropThen T.Text CExp
  deriving (Show, Eq)
