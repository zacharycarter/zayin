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
  , logDebugM
  ) where

import Control.Monad (foldM)
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Debug.Trace as Debug
import qualified Zayin.BoundExpr as BE
import Zayin.Literals (Literal(..))

-- Core data types
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

logDebugM :: Show a => String -> a -> Fresh a
logDebugM prefix x = do
  Debug.traceM (prefix ++ ": " ++ show x)
  return x

-- Explicit debug logging for each stage
logStage :: String -> Fresh a -> Fresh a
logStage stage action = do
  Debug.traceM $ "=== Starting " ++ stage ++ " ==="
  result <- action
  Debug.traceM $ "=== Completed " ++ stage ++ " ==="
  return result

freshName :: T.Text -> Fresh T.Text
freshName prefix = do
  (Gen f) <- get
  let (g1, g2) = (Gen (\x -> f (2 * x)), Gen (\x -> f (2 * x + 1)))
  put g2
  return $ prefix <> "$" <> T.pack (show (f 0))

rewrite :: (Expr -> Fresh Expr) -> Expr -> Fresh Expr
rewrite f expr = do
  logDebugM "rewrite before" expr
  processed <- case expr of
    EVar {} -> return expr
    ELit {} -> return expr
    EBuiltinIdent {} -> return expr
    EIf c t e -> do
      logDebugM "rewriting if condition" c
      c' <- rewrite f c
      t' <- rewrite f t
      e' <- rewrite f e
      return $ EIf c' t' e'
    ESet n e -> do
      logDebugM "rewriting set" (n, e)
      e' <- rewrite f e
      let result = ESet n e'
      logDebugM "set after rewrite" result
      return result
    ELet bs body -> do
      logDebugM "rewriting let bindings" bs
      bs' <- mapM (\(n,e) -> do
        e' <- rewrite f e
        return (n, e')) bs
      body' <- rewriteBody f body
      return $ ELet bs' body'
    ELam ps body -> do
      logDebugM "rewriting lambda body" body
      body' <- rewriteBody f body
      return $ ELam ps body'
    EApp fun args -> do
      logDebugM "rewriting application" (fun, args)
      fun' <- rewrite f fun
      args' <- mapM (rewrite f) args
      return $ EApp fun' args'
  result <- f processed
  logDebugM "rewrite after" result
  return result

rewriteBody :: (Expr -> Fresh Expr) -> ExprBody -> Fresh ExprBody
rewriteBody f (ExprBody exprs final) = do
  logDebugM "rewriting body exprs" exprs
  exprs' <- mapM (\case
    Def n e -> do
      logDebugM "rewriting define" (n, e)
      e' <- rewrite f e
      return $ Def n e'
    Expr e -> do
      logDebugM "rewriting body expr" e
      e' <- rewrite f e
      return $ Expr e') exprs
  final' <- rewrite f final
  let result = ExprBody exprs' final'
  logDebugM "rewriteBody result" result
  return result

pullDefines :: ExprBody -> Fresh ExprBody
pullDefines (ExprBody exprs final) = do
  logDebugM "pullDefines input" exprs

  -- Transform defines into sets and collect names
  let (defines, bodyExprs) = foldr (\expr (defs, body) ->
        case expr of
          Def name e ->
            let setExpr = Expr (ESet name e)
            in Debug.trace ("converting define to set: " ++ show (name, e, setExpr))
               (name:defs, setExpr:body)
          other -> (defs, other:body)
        ) ([], []) exprs

  logDebugM "Defines found" defines
  logDebugM "Body with sets" bodyExprs

  -- Create let bindings and wrap body
  let letBindings = [(name, ELit LNil) | name <- defines]
  let bodyWithSets = ExprBody bodyExprs final
  let result = ExprBody [] (ELet letBindings bodyWithSets)

  logDebugM "pullDefines result" result
  return result

liftDefines :: Expr -> Fresh Expr
liftDefines expr = do
  Debug.traceM "=== Starting liftDefines ==="
  result <- rewrite (\case
    ELet bindings body -> do
      logDebugM "lifting defines in let" body
      body' <- pullDefines body
      return $ ELet bindings body'
    ELam params body -> do
      logDebugM "lifting defines in lambda" body
      body' <- pullDefines body
      return $ ELam params body'
    e -> return e) expr
  Debug.traceM "=== Completed liftDefines ==="
  return result

removeLet :: Expr -> Fresh Expr
removeLet expr = do
  Debug.traceM "=== Starting removeLet ==="
  result <- rewrite (\case
    ELet bindings body -> do
      logDebugM "removing let" (bindings, body)
      let (names, exprs) = unzip bindings
      let result = EApp (ELam names body) exprs
      logDebugM "let removal result" result
      return result
    e -> return e) expr
  Debug.traceM "=== Completed removeLet ==="
  return result

toBoundExprM :: Expr -> Fresh BE.BExpr
toBoundExprM expr = logStage "toBoundExprM" $ do
  logDebugM "initial expr" expr
  expr' <- liftDefines expr
  logDebugM "after define lifting" expr'
  expr'' <- removeLet expr'
  logDebugM "after let removal" expr''
  toBoundExprInner expr'' Map.empty

toBoundExprInner :: Expr -> Map.Map T.Text T.Text -> Fresh BE.BExpr
toBoundExprInner expr env = do
  logDebugM "toBoundExprInner" (expr, env)
  case expr of
    EVar name -> do
      let boundName = Map.findWithDefault name name env
      return $ BE.Var boundName

    ELit lit ->
      return $ BE.Lit lit

    EBuiltinIdent name ->
      return $ BE.BuiltinIdent name

    EIf cond t f -> do
      c' <- toBoundExprInner cond env
      t' <- toBoundExprInner t env
      f' <- toBoundExprInner f env
      return $ BE.If c' t' f'

    ESet name val -> do
      logDebugM "converting set" (name, val)
      -- Use the bound name from the environment if it exists, otherwise generate fresh
      let boundName = Map.findWithDefault name name env
      val' <- toBoundExprInner val env
      let result = BE.Set boundName val'
      logDebugM "set conversion result" result
      return result

    ELam params body -> do
      -- First generate fresh names for all parameters
      freshParams <- mapM (\p -> do
                           fresh <- freshName p
                           return (p, fresh)) params

      -- Create new environment with all fresh parameter names
      let paramEnv = foldr (\(p, fresh) e -> Map.insert p fresh e) env freshParams

      -- Process the body expressions
      result <- case (bodyExprs body, params) of
        ([], []) -> do
          unused <- freshName "_unused"
          body' <- toBoundExprInner (finalExpr body) env
          return $ BE.Lam unused body'
        (exprs, _) -> do
          let processExprs [] final = toBoundExprInner final paramEnv
              processExprs (Expr e:rest) final = do
                unused <- freshName "_unused"
                e' <- toBoundExprInner e paramEnv
                final' <- processExprs rest final
                return $ BE.App (BE.Lam unused final') e'
              processExprs (Def n e:rest) final = do
                -- Use the bound name from paramEnv if it exists
                let boundName = Map.findWithDefault n n paramEnv
                e' <- toBoundExprInner e paramEnv
                final' <- processExprs rest final
                return $ BE.App (BE.Lam boundName final') e'

          body' <- processExprs (bodyExprs body) (finalExpr body)
          -- Use the fresh names we generated earlier
          return $ foldr (\(_, fresh) acc -> BE.Lam fresh acc)
                        body'
                        freshParams
      return result

    EApp f [] -> do
      f' <- toBoundExprInner f env
      return $ BE.App f' (BE.Lit LNil)

    EApp f (a:as) -> do
      f' <- toBoundExprInner f env
      a' <- toBoundExprInner a env
      foldM (\acc e -> BE.App acc <$> toBoundExprInner e env) (BE.App f' a') as

    ELet _ _ ->
      error "Let expressions should have been removed"
