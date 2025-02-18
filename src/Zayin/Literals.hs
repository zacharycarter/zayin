module Zayin.Literals (Literal (..)) where

import qualified Data.Text as T

data Literal
  = LString T.Text
  | LInt Integer
  | LFloat Float
  | LNil
  deriving (Show, Eq)
