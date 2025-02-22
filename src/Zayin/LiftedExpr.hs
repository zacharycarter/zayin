{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Zayin.LiftedExpr (LExpr (..), LiftedLambda (..)) where

import Data.Data (Data, Typeable)
import Data.HashMap.Strict (HashMap)
import Data.Set (Set)
import qualified Data.Text as T
import Zayin.Literals (Literal(..))

deriving instance Data Literal

data LExpr
  = Var T.Text
  | Lit Literal
  | BuiltinIdent T.Text
  | SetThen T.Text LExpr LExpr
  | If LExpr LExpr LExpr
  | Lifted Int
  | CallOne LExpr LExpr
  | CallTwo LExpr LExpr LExpr
  deriving (Show, Eq, Data, Typeable)

data LiftedLambda = LiftedLambda
  { lambdaId :: Int,
    params :: [T.Text],
    freeVars :: Set T.Text,
    body :: LExpr
  }
  deriving (Show, Eq, Data, Typeable)
