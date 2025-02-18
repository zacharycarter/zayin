{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Zayin.AST (Expr (..), Fresh (..), Gen (..), ExprBody (..), ExprBodyExpr(..), freshName, toBoundExprM) where

import Control.Monad (foldM)
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Unique (hashUnique, newUnique)
import qualified Zayin.BoundExpr as BE
import Zayin.Literals (Literal (..))

data Expr
  = EVar T.Text
  | ELit Literal
  | EBuiltinIdent T.Text
  | EIf Expr Expr Expr
  | ESet T.Text Expr
  | ELet [(T.Text, Expr)] ExprBody
  | ELam [T.Text] ExprBody
  | EApp Expr [Expr]
  deriving (Show, Eq)

data ExprBodyExpr
  = Def T.Text Expr
  | Expr Expr
  deriving (Show, Eq)

data ExprBody = ExprBody
  { bodyExprs :: [ExprBodyExpr],
    finalExpr :: Expr
  }
  deriving (Show, Eq)

data Gen = Gen (Int -> Int)

type Fresh a = State Gen a

freshName :: T.Text -> Fresh T.Text
freshName prefix = do
  (Gen f) <- get
  let (g1, g2) = (Gen (\x -> f (2 * x)), Gen (\x -> f (2 * x + 1)))
  put g2
  return $ prefix <> "$" <> T.pack (show (f 0))

toBoundExprM :: Expr -> Fresh BE.BExpr
toBoundExprM expr = do
  exp <- removeLet =<< liftDefines expr
  toBoundExprInner exp Map.empty

toBoundExprInner :: Expr -> Map.Map T.Text T.Text -> Fresh BE.BExpr
toBoundExprInner expr env = case expr of
  EVar name -> return $ BE.Var (Map.findWithDefault name name env)
  ELit lit -> return $ BE.Lit lit
  EBuiltinIdent name -> return $ BE.BuiltinIdent name
  EIf cond t f ->
    BE.If
      <$> toBoundExprInner cond env
      <*> toBoundExprInner t env
      <*> toBoundExprInner f env
  ESet name val -> BE.Set (Map.findWithDefault name name env) <$> toBoundExprInner val env
  ELam [] body -> do
    unused1 <- freshName "_unused"
    unused0 <- freshName "_unused"
    finalExp <- toBoundExprInner (finalExpr body) env
    return $ BE.Lam unused1 (BE.App (BE.Lam unused0 finalExp) (BE.Lit LNil))
  ELam (p : ps) body -> do
    freshP <- freshName p
    let newEnv = Map.insert p freshP env
    finalExp <- toBoundExprInner (finalExpr body) env
    body' <- foldM (buildLambda newEnv) finalExp ps
    return $ BE.Lam freshP body'
  EApp f [] -> BE.App <$> toBoundExprInner f env <*> pure (BE.Lit LNil)
  EApp f (a : as) -> do
    f' <- toBoundExprInner f env
    a' <- toBoundExprInner a env
    foldM (\acc e -> flip BE.App <$> toBoundExprInner e env <*> pure acc) (BE.App f' a') as
  ELet _ _ -> error "Let expressions should be removed by removeLet"
  where
    buildLambda env acc p = do
      freshP <- freshName p
      let newEnv = Map.insert p freshP env
      return $ BE.Lam freshP acc

removeLet :: Expr -> Fresh Expr
removeLet expr = case expr of
  ELet bindings body -> do
    let (names, exprs) = unzip bindings
    body' <- removeLet (finalExpr body)
    params' <- mapM removeLet exprs
    return $ EApp (ELam names (ExprBody [] body')) params'
  _ -> return expr

liftDefines :: Expr -> Fresh Expr
liftDefines expr = case expr of
  ELet bindings body -> ELet bindings <$> liftDefinesBody body
  ELam params body -> ELam params <$> liftDefinesBody body
  _ -> return expr

liftDefinesBody :: ExprBody -> Fresh ExprBody
liftDefinesBody (ExprBody exprs final) = do
  let (defs, others) = foldr collectDefs ([], []) exprs
  return $
    ExprBody
      ( map
          ( \case
              Def name expr -> Expr (ESet name expr)
              e -> e
          )
          others
      )
      final
  where
    collectDefs (Def name _) (defs, others) = (name : defs, Def name (ELit LNil) : others)
    collectDefs e (defs, others) = (defs, e : others)
