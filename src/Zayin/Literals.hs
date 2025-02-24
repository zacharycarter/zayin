module Zayin.Literals (Literal (..)) where

import qualified Data.Text as T

data Literal
  = LString T.Text
  | LInt Integer
  | LFloat Float
  | LBool Bool
  | LNil
  deriving (Show, Eq)
