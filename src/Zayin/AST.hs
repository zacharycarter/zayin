{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Zayin.AST
  ( Expr (..),
    Fresh,
    Gen (..),
    ExprBody (..),
    ExprBodyExpr (..),
    freshName,
    toBoundExprM,
    logDebugM,
    runFresh,
  )
where

import Control.Monad (foldM, when)
import Control.Monad.Logger
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Zayin.BoundExpr as BE
import Zayin.Literals (Literal (..))

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
  { bodyExprs :: [ExprBodyExpr],
    finalExpr :: Expr
  }
  deriving (Show, Eq)

data Gen = Gen (Int -> Int)

-- Update Fresh monad to include logging capabilities
type Fresh a = LoggingT (StateT Gen IO) a

-- Run the Fresh monad computations with logging to stderr
runFresh :: Fresh a -> Gen -> IO (a, Gen)
runFresh action gen = runStateT (runStderrLoggingT action) gen

-- Conditionally log debug messages with proper formatting
logDebugM :: (Show a) => Bool -> String -> a -> Fresh a
logDebugM debugMode prefix x = do
  when debugMode $
    logDebugN $
      T.pack $
        prefix ++ ": " ++ show x
  return x

-- Explicit debug logging for each stage
logStage :: Bool -> String -> Fresh a -> Fresh a
logStage debugMode stage action = do
  when debugMode $
    logDebugN $
      T.pack $
        "=== Starting " ++ stage ++ " ==="
  result <- action
  when debugMode $
    logDebugN $
      T.pack $
        "=== Completed " ++ stage ++ " ==="
  return result

freshName :: T.Text -> Fresh T.Text
freshName prefix = do
  (Gen f) <- get
  let (g1, g2) = (Gen (\x -> f (2 * x)), Gen (\x -> f (2 * x + 1)))
  put g2
  return $ prefix <> "$" <> T.pack (show (f 0))

rewrite :: Bool -> (Expr -> Fresh Expr) -> Expr -> Fresh Expr
rewrite debugMode f expr = do
  logDebugM debugMode "rewrite before" expr
  processed <- case expr of
    EVar {} -> return expr
    ELit {} -> return expr
    EBuiltinIdent {} -> return expr
    EIf c t e -> do
      logDebugM debugMode "rewriting if condition" c
      c' <- rewrite debugMode f c
      t' <- rewrite debugMode f t
      e' <- rewrite debugMode f e
      return $ EIf c' t' e'
    ESet n e -> do
      logDebugM debugMode "rewriting set" (n, e)
      e' <- rewrite debugMode f e
      let result = ESet n e'
      logDebugM debugMode "set after rewrite" result
      return result
    ELet bs body -> do
      logDebugM debugMode "rewriting let bindings" bs
      bs' <-
        mapM
          ( \(n, e) -> do
              e' <- rewrite debugMode f e
              return (n, e')
          )
          bs
      body' <- rewriteBody debugMode f body
      return $ ELet bs' body'
    ELam ps body -> do
      logDebugM debugMode "rewriting lambda body" body
      body' <- rewriteBody debugMode f body
      return $ ELam ps body'
    EApp fun args -> do
      logDebugM debugMode "rewriting application" (fun, args)
      fun' <- rewrite debugMode f fun
      args' <- mapM (rewrite debugMode f) args
      return $ EApp fun' args'
  result <- f processed
  logDebugM debugMode "rewrite after" result
  return result

rewriteBody :: Bool -> (Expr -> Fresh Expr) -> ExprBody -> Fresh ExprBody
rewriteBody debugMode f (ExprBody exprs final) = do
  logDebugM debugMode "rewriting body exprs" exprs
  exprs' <-
    mapM
      ( \case
          Def n e -> do
            logDebugM debugMode "rewriting define" (n, e)
            e' <- rewrite debugMode f e
            return $ Def n e'
          Expr e -> do
            logDebugM debugMode "rewriting body expr" e
            e' <- rewrite debugMode f e
            return $ Expr e'
      )
      exprs
  final' <- rewrite debugMode f final
  let result = ExprBody exprs' final'
  logDebugM debugMode "rewriteBody result" result
  return result

pullDefines :: Bool -> ExprBody -> Fresh ExprBody
pullDefines debugMode (ExprBody exprs final) = do
  logDebugM debugMode "pullDefines input" exprs

  -- Transform defines into sets and collect names
  let (defines, bodyExprs) =
        foldr
          ( \expr (defs, body) ->
              case expr of
                Def name e ->
                  let setExpr = Expr (ESet name e)
                   in (name : defs, setExpr : body)
                other -> (defs, other : body)
          )
          ([], [])
          exprs

  -- Log the defines we found (moved outside of foldr)
  when debugMode $
    mapM_ (\def -> logDebugN $ T.pack $ "converted define to set: " ++ show def) defines

  logDebugM debugMode "Defines found" defines
  logDebugM debugMode "Body with sets" bodyExprs

  -- Create let bindings and wrap body
  let letBindings = [(name, ELit LNil) | name <- defines]
  let bodyWithSets = ExprBody bodyExprs final
  let result = ExprBody [] (ELet letBindings bodyWithSets)

  logDebugM debugMode "pullDefines result" result
  return result

liftDefines :: Bool -> Expr -> Fresh Expr
liftDefines debugMode expr = do
  when debugMode $
    logDebugN "=== Starting liftDefines ==="
  result <-
    rewrite
      debugMode
      ( \case
          ELet bindings body -> do
            logDebugM debugMode "lifting defines in let" body
            body' <- pullDefines debugMode body
            return $ ELet bindings body'
          ELam params body -> do
            logDebugM debugMode "lifting defines in lambda" body
            body' <- pullDefines debugMode body
            return $ ELam params body'
          e -> return e
      )
      expr
  when debugMode $
    logDebugN "=== Completed liftDefines ==="
  return result

removeLet :: Bool -> Expr -> Fresh Expr
removeLet debugMode expr = do
  when debugMode $
    logDebugN "=== Starting removeLet ==="
  result <-
    rewrite
      debugMode
      ( \case
          ELet bindings body -> do
            logDebugM debugMode "removing let" (bindings, body)
            let (names, exprs) = unzip bindings
            let result = EApp (ELam names body) exprs
            logDebugM debugMode "let removal result" result
            return result
          e -> return e
      )
      expr
  when debugMode $
    logDebugN "=== Completed removeLet ==="
  return result

toBoundExprM :: Bool -> Expr -> Fresh BE.BExpr
toBoundExprM debugMode expr = logStage debugMode "toBoundExprM" $ do
  logDebugM debugMode "initial expr" expr
  expr' <- liftDefines debugMode expr
  logDebugM debugMode "after define lifting" expr'
  expr'' <- removeLet debugMode expr'
  logDebugM debugMode "after let removal" expr''
  toBoundExprInner debugMode expr'' Map.empty

toBoundExprInner :: Bool -> Expr -> Map.Map T.Text T.Text -> Fresh BE.BExpr
toBoundExprInner debugMode expr env = do
  logDebugM debugMode "toBoundExprInner" (expr, env)
  case expr of
    EVar name -> do
      let boundName = Map.findWithDefault name name env
      return $ BE.Var boundName
    ELit lit ->
      return $ BE.Lit lit
    EBuiltinIdent name ->
      return $ BE.BuiltinIdent name
    EIf cond t f -> do
      c' <- toBoundExprInner debugMode cond env
      t' <- toBoundExprInner debugMode t env
      f' <- toBoundExprInner debugMode f env
      return $ BE.If c' t' f'
    ESet name val -> do
      logDebugM debugMode "converting set" (name, val)
      -- Use the bound name from the environment if it exists, otherwise generate fresh
      let boundName = Map.findWithDefault name name env
      val' <- toBoundExprInner debugMode val env
      let result = BE.Set boundName val'
      logDebugM debugMode "set conversion result" result
      return result
    ELam params body -> do
      -- First generate fresh names for all parameters
      freshParams <-
        mapM
          ( \p -> do
              fresh <- freshName p
              return (p, fresh)
          )
          params

      -- Create new environment with all fresh parameter names
      let paramEnv = foldr (\(p, fresh) e -> Map.insert p fresh e) env freshParams

      -- Process the body expressions
      result <- case (bodyExprs body, params) of
        ([], []) -> do
          unused <- freshName "_unused"
          body' <- toBoundExprInner debugMode (finalExpr body) env
          return $ BE.Lam unused body'
        (exprs, _) -> do
          let processExprs [] final = toBoundExprInner debugMode final paramEnv
              processExprs (Expr e : rest) final = do
                unused <- freshName "_unused"
                e' <- toBoundExprInner debugMode e paramEnv
                final' <- processExprs rest final
                return $ BE.App (BE.Lam unused final') e'
              processExprs (Def n e : rest) final = do
                -- Use the bound name from paramEnv if it exists
                let boundName = Map.findWithDefault n n paramEnv
                e' <- toBoundExprInner debugMode e paramEnv
                final' <- processExprs rest final
                return $ BE.App (BE.Lam boundName final') e'

          body' <- processExprs (bodyExprs body) (finalExpr body)
          -- Use the fresh names we generated earlier
          return $
            foldr
              (\(_, fresh) acc -> BE.Lam fresh acc)
              body'
              freshParams
      return result
    EApp f [] -> do
      f' <- toBoundExprInner debugMode f env
      return $ BE.App f' (BE.Lit LNil)
    EApp f (a : as) -> do
      f' <- toBoundExprInner debugMode f env
      a' <- toBoundExprInner debugMode a env
      foldM (\acc e -> BE.App acc <$> toBoundExprInner debugMode e env) (BE.App f' a') as
    ELet _ _ ->
      error "Let expressions should have been removed"
