{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Zayin.AST
  ( Expr(..)
  , Fresh
  , Gen(..)
  , ExprBody(..)
  , ExprBodyExpr(..)
  , freshName
  , toBoundExprM
  , removeLet
  , liftDefines
  ) where

import Control.Monad (foldM)
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Zayin.BoundExpr as BE
import Zayin.Literals (Literal(..))

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
  { bodyExprs :: [ExprBodyExpr]
  , finalExpr :: Expr
  }
  deriving (Show, Eq)

data Gen = Gen (Int -> Int)
type Fresh a = State Gen a

freshName :: T.Text -> Fresh T.Text
freshName prefix = do
  (Gen f) <- get
  -- Update state by splitting the counter (simple scheme)
  let (g1, g2) = (Gen (\x -> f (2 * x)), Gen (\x -> f (2 * x + 1)))
  put g2
  return $ prefix <> "$" <> T.pack (show (f 0))

--------------------------------------------------------------------------------
-- Generic Recursive Rewrite
--------------------------------------------------------------------------------

-- Recursively traverses the entire Expr AST, applying a transformation
-- function f at every node.
rewrite :: (Expr -> Fresh Expr) -> Expr -> Fresh Expr
rewrite f expr = do
  expr' <- case expr of
    EVar _           -> return expr
    ELit _           -> return expr
    EBuiltinIdent _  -> return expr
    EIf cond t f'   -> EIf <$> rewrite f cond <*> rewrite f t <*> rewrite f f'
    ESet name val    -> ESet name <$> rewrite f val
    ELet bindings body -> do
         bindings' <- mapM (\(n,e) -> do e' <- rewrite f e; return (n,e')) bindings
         body' <- rewriteBody f body
         return $ ELet bindings' body'
    ELam params body -> ELam params <$> rewriteBody f body
    EApp e args     -> EApp <$> rewrite f e <*> mapM (rewrite f) args
  f expr'

rewriteBody :: (Expr -> Fresh Expr) -> ExprBody -> Fresh ExprBody
rewriteBody f (ExprBody exprs final) = do
  exprs' <- mapM (rewriteBodyExpr f) exprs
  final' <- rewrite f final
  return $ ExprBody exprs' final'

rewriteBodyExpr :: (Expr -> Fresh Expr) -> ExprBodyExpr -> Fresh ExprBodyExpr
rewriteBodyExpr f (Def name e) = Def name <$> rewrite f e
rewriteBodyExpr f (Expr e)     = Expr <$> rewrite f e

--------------------------------------------------------------------------------
-- Updated Let Removal and Define Lifting
--------------------------------------------------------------------------------

-- removeLet now uses the generic rewrite to transform any ELet anywhere
removeLet :: Expr -> Fresh Expr
removeLet = rewrite removeLetStep
  where
    removeLetStep :: Expr -> Fresh Expr
    removeLetStep (ELet bindings body) = do
      let (names, exprs) = unzip bindings
      body' <- removeLet (finalExpr body)
      args' <- mapM removeLet exprs
      return $ EApp (ELam names (ExprBody [] body')) args'
    removeLetStep e = return e

-- liftDefines now uses rewrite to traverse all nodes
liftDefines :: Expr -> Fresh Expr
liftDefines = rewrite liftDefinesStep
  where
    liftDefinesStep :: Expr -> Fresh Expr
    liftDefinesStep (ELet bindings body) = do
       body' <- liftDefinesBody body
       return $ ELet bindings body'
    liftDefinesStep (ELam params body) = do
       body' <- liftDefinesBody body
       return $ ELam params body'
    liftDefinesStep e = return e

liftDefinesBody :: ExprBody -> Fresh ExprBody
liftDefinesBody (ExprBody exprs final) = do
  let (defs, others) = foldr collectDefs ([], []) exprs
      -- For every definition in the original body, convert it into a set-expression.
      body' = map (\case
                     Def name expr -> Expr (ESet name expr)
                     e -> e
                  ) others
      -- For every defined name, bind it to void.
      letBindings = map (\n -> (n, ELit LNil)) defs
      -- Wrap the transformed body and final expression in a let binding.
      letExpr = ELet letBindings (ExprBody body' final)
  -- Return an ExprBody with an empty bodyExprs and letExpr as the final expression.
  return $ ExprBody [] letExpr
  where
    -- <<-- CHANGE THIS LINE: Preserve the original expression!
    collectDefs (Def name e) (ds, es) = (name : ds, Def name e : es)
    collectDefs e (ds, es)             = (ds, e : es)

--------------------------------------------------------------------------------
-- Conversion to Bound Expression (unchanged)
--------------------------------------------------------------------------------

toBoundExprM :: Expr -> Fresh BE.BExpr
toBoundExprM expr = do
  exp' <- removeLet =<< liftDefines expr
  toBoundExprInner exp' Map.empty

toBoundExprInner :: Expr -> Map.Map T.Text T.Text -> Fresh BE.BExpr
toBoundExprInner expr env = case expr of
  EVar name -> return $ BE.Var (Map.findWithDefault name name env)
  ELit lit -> return $ BE.Lit lit
  EBuiltinIdent name -> return $ BE.BuiltinIdent name
  EIf cond t f ->
    BE.If <$> toBoundExprInner cond env
          <*> toBoundExprInner t env
          <*> toBoundExprInner f env
  ESet name val -> BE.Set (Map.findWithDefault name name env) <$> toBoundExprInner val env
  ELam [] body -> do
    unused1 <- freshName "_unused"
    unused0 <- freshName "_unused"
    finalExp <- toBoundExprInner (finalExpr body) env
    return $ BE.Lam unused1 (BE.App (BE.Lam unused0 finalExp) (BE.Lit LNil))
  ELam (p:ps) body -> do
    freshP <- freshName p
    let newEnv = Map.insert p freshP env
    finalExp <- toBoundExprInner (finalExpr body) env
    body' <- foldM (buildLambda newEnv) finalExp ps
    return $ BE.Lam freshP body'
  EApp f [] -> BE.App <$> toBoundExprInner f env <*> pure (BE.Lit LNil)
  EApp f (a:as) -> do
    f' <- toBoundExprInner f env
    a' <- toBoundExprInner a env
    foldM (\acc e -> flip BE.App <$> toBoundExprInner e env <*> pure acc) (BE.App f' a') as
  ELet _ _ -> error "Let expressions should be removed by removeLet"
  where
    buildLambda env acc p = do
      freshP <- freshName p
      let newEnv = Map.insert p freshP env
      return $ BE.Lam freshP acc
