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

-- Monadic logging helper
logDebugM :: Show a => String -> a -> Fresh a
logDebugM prefix x = do
  Debug.trace (prefix ++ ": " ++ show x) `seq` return x

-- Fresh name generation
freshName :: T.Text -> Fresh T.Text
freshName prefix = do
  (Gen f) <- get
  let (g1, g2) = (Gen (\x -> f (2 * x)), Gen (\x -> f (2 * x + 1)))
  put g2
  name <- return $ prefix <> "$" <> T.pack (show (f 0))
  logDebugM "Generated fresh name" name

-- Generic recursive rewrite
rewrite :: (Expr -> Fresh Expr) -> Expr -> Fresh Expr
rewrite f expr = do
  expr' <- logDebugM "rewrite input" expr
  transformed <- case expr' of
    EVar _ -> return expr
    ELit _ -> return expr
    EBuiltinIdent _ -> return expr
    EIf cond t f' -> do
      cond' <- rewrite f cond
      t' <- rewrite f t
      f'' <- rewrite f f'
      return $ EIf cond' t' f''
    ESet name val -> do
      val' <- rewrite f val
      return $ ESet name val'
    ELet bindings body -> do
      bindings' <- mapM (\(n,e) -> do
        e' <- rewrite f e
        return (n,e')) bindings
      body' <- rewriteBody f body
      return $ ELet bindings' body'
    ELam params body -> do
      body' <- rewriteBody f body
      return $ ELam params body'
    EApp e args -> do
      e' <- rewrite f e
      args' <- mapM (rewrite f) args
      return $ EApp e' args'
  result <- f transformed
  logDebugM "rewrite output" result

rewriteBody :: (Expr -> Fresh Expr) -> ExprBody -> Fresh ExprBody
rewriteBody f (ExprBody exprs final) = do
  exprs' <- logDebugM "rewriteBody exprs input" exprs >>= mapM (rewriteBodyExpr f)
  final' <- logDebugM "rewriteBody final input" final >>= rewrite f
  result <- return $ ExprBody exprs' final'
  logDebugM "rewriteBody output" result

rewriteBodyExpr :: (Expr -> Fresh Expr) -> ExprBodyExpr -> Fresh ExprBodyExpr
rewriteBodyExpr f expr = do
  expr' <- logDebugM "rewriteBodyExpr input" expr
  result <- case expr' of
    Def name e -> Def name <$> rewrite f e
    Expr e -> Expr <$> rewrite f e
  logDebugM "rewriteBodyExpr output" result

-- Let removal
removeLet :: Expr -> Fresh Expr
removeLet expr = do
  expr' <- logDebugM "removeLet input" expr
  result <- rewrite removeLetStep expr'
  logDebugM "removeLet output" result
  where
    removeLetStep :: Expr -> Fresh Expr
    removeLetStep e = do
      e' <- logDebugM "removeLetStep input" e
      result <- case e' of
        ELet bindings body -> do
          let (names, exprs) = unzip bindings
          body' <- removeLet (finalExpr body)
          args' <- mapM removeLet exprs
          return $ EApp (ELam names (ExprBody [] body')) args'
        other -> return other
      logDebugM "removeLetStep output" result

-- Define lifting
liftDefines :: Expr -> Fresh Expr
liftDefines expr = do
  expr' <- logDebugM "liftDefines input" expr
  result <- rewrite liftDefinesStep expr'
  logDebugM "liftDefines output" result
  where
    liftDefinesStep :: Expr -> Fresh Expr
    liftDefinesStep e = do
      e' <- logDebugM "liftDefinesStep input" e
      result <- case e' of
        ELet bindings body -> do
          body' <- liftDefinesBody body
          return $ ELet bindings body'
        ELam params body -> do
          body' <- liftDefinesBody body
          return $ ELam params body'
        other -> return other
      logDebugM "liftDefinesStep output" result

liftDefinesBody :: ExprBody -> Fresh ExprBody
liftDefinesBody (ExprBody exprs final) = do
  exprs' <- logDebugM "liftDefinesBody exprs input" exprs
  final' <- logDebugM "liftDefinesBody final input" final

  let (defs, others) = foldr collectDefs ([], []) exprs'
  defs' <- logDebugM "Collected definitions" defs
  others' <- logDebugM "Remaining expressions" others

  -- Log each define transformation
  mapM_ (\case
          Def name expr -> logDebugM ("Processing define for " ++ show name) expr
          Expr e -> logDebugM "Processing expression" e
        ) exprs'

  -- Transform defines into sets
  body' <- mapM (\case
                  Def name expr -> do
                    logDebugM ("Converting define to set for " ++ show name) expr
                    return $ Expr (ESet name expr)
                  e -> return e
                ) exprs'

  body'' <- logDebugM "Body after define transformation" body'

  -- Create let bindings
  let letBindings = map (\n -> (n, ELit LNil)) defs'
  bindings' <- logDebugM "Created let bindings" letBindings

  -- Create final let expression
  let letExpr = ELet bindings' (ExprBody body'' final')
  expr' <- logDebugM "Created let expression" letExpr

  -- Create final body
  result <- return $ ExprBody [] expr'
  logDebugM "liftDefinesBody final output" result
  where
    collectDefs (Def name e) (ds, es) =
      let pair = (name : ds, e : es)
      in Debug.trace ("collectDefs for " ++ show name ++ ": " ++ show pair) pair
    collectDefs (Expr e) (ds, es) = (ds, e : es)

-- Bound expression conversion
toBoundExprM :: Expr -> Fresh BE.BExpr
toBoundExprM expr = do
  expr' <- logDebugM "toBoundExprM initial input" expr

  -- Log and perform define lifting
  _ <- logDebugM "Starting define lifting on" expr'
  lifted <- liftDefines expr'
  _ <- logDebugM "After define lifting" lifted

  -- Log and perform let removal
  _ <- logDebugM "Starting let removal on" lifted
  noLet <- removeLet lifted
  _ <- logDebugM "After let removal" noLet

  -- Log and perform final conversion
  _ <- logDebugM "Starting bound conversion on" noLet
  result <- toBoundExprInner noLet Map.empty
  logDebugM "toBoundExprM final output" result

toBoundExprInner :: Expr -> Map.Map T.Text T.Text -> Fresh BE.BExpr
toBoundExprInner expr env = do
  expr' <- logDebugM "toBoundExprInner input" expr
  env' <- logDebugM "Current environment" env
  result <- case expr' of
    EVar name -> do
      let boundName = Map.findWithDefault name name env'
      logDebugM "Variable binding" boundName >>= \n -> return $ BE.Var n

    ELit lit -> return $ BE.Lit lit

    EBuiltinIdent name -> return $ BE.BuiltinIdent name

    EIf cond t f -> do
      cond' <- logDebugM "If condition" cond >>= \c -> toBoundExprInner c env'
      t' <- logDebugM "If true branch" t >>= \tb -> toBoundExprInner tb env'
      f' <- logDebugM "If false branch" f >>= \fb -> toBoundExprInner fb env'
      logDebugM "Constructed if" (BE.If cond' t' f')

    ESet name val -> do
      val' <- logDebugM "Set value" val >>= \v -> toBoundExprInner v env'
      let boundName = Map.findWithDefault name name env'
      result <- return $ BE.Set boundName val'
      logDebugM "Constructed set" result

    ELam [] body -> do
      unused1 <- freshName "_unused"
      unused0 <- freshName "_unused"
      names <- logDebugM "Empty lambda names" (unused0, unused1)
      finalExp <- toBoundExprInner (finalExpr body) env'
      let result = BE.Lam unused1 (BE.App (BE.Lam unused0 finalExp) (BE.Lit LNil))
      logDebugM "Constructed empty lambda" result

    ELam params body -> do
      freshParams <- mapM freshName params
      freshParams' <- logDebugM "Lambda parameters" (params, freshParams)
      let paramBindings = Map.fromList (zip params freshParams)
          newEnv = Map.union paramBindings env'

      newEnv' <- logDebugM "New lambda environment" newEnv

      bodyExprs <- mapM (\case
          Def name e -> do
            e' <- logDebugM "Lambda definition" (name, e) >>= \(_, expr) ->
                  toBoundExprInner expr newEnv'
            let boundName = Map.findWithDefault name name newEnv'
            result <- return $ BE.Set boundName e'
            logDebugM "Constructed lambda definition" result
          Expr e -> do
            e' <- logDebugM "Lambda body expression" e >>= \expr ->
                  toBoundExprInner expr newEnv'
            logDebugM "Constructed lambda expression" e'
        ) (bodyExprs body)

      bodyExprs' <- logDebugM "Lambda body expressions" bodyExprs
      finalExpr <- toBoundExprInner (finalExpr body) newEnv'

      let result = foldr BE.Lam finalExpr freshParams
      logDebugM "Constructed lambda" result

    EApp f [] -> do
      f' <- logDebugM "Nullary application function" f >>= \func ->
            toBoundExprInner func env'
      result <- return $ BE.App f' (BE.Lit LNil)
      logDebugM "Constructed nullary application" result

    EApp f (a:as) -> do
      f' <- logDebugM "Application function" f >>= \func ->
            toBoundExprInner func env'
      a' <- logDebugM "First argument" a >>= \arg -> toBoundExprInner arg env'
      as' <- logDebugM "Remaining arguments" as
      result <- foldM (\acc e -> do
                   e' <- logDebugM "Processing argument" e >>= \arg ->
                        toBoundExprInner arg env'
                   return $ BE.App acc e'
                ) (BE.App f' a') as'
      logDebugM "Constructed application" result

    ELet _ _ -> error "Let expressions should be removed by removeLet"

  logDebugM "toBoundExprInner output" result
