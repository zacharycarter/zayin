{-# LANGUAGE OverloadedStrings #-}

module Zayin.Transforms (toFExprM) where

import Control.Monad (foldM, when)
import Control.Monad.Logger
import Control.Monad.State
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Zayin.AST (Fresh, Gen (..), freshName)
import qualified Zayin.BoundExpr as BE
import qualified Zayin.CPS as CPS
import qualified Zayin.FlatExpr as FE
import Zayin.Literals

toFExprM :: Bool -> BE.BExpr -> CPS.AExp -> Fresh FE.FExpr
toFExprM debugMode e k = do
  when debugMode $
    logDebugN $
      T.pack $
        "\n=== Starting CPS Transformation ===\n"
          ++ "Initial bound expression: "
          ++ show e
          ++ "\n"
          ++ "Initial continuation: "
          ++ show k

  -- Use the debug-aware functions
  c_expr <- tC debugMode e k

  when debugMode $
    logDebugN $
      T.pack $
        "\nAfter tC transformation:\n" ++ show c_expr

  -- Use the debug-aware functions for pure transformations
  let result = intotoFExpr debugMode c_expr

  when debugMode $
    logDebugN $
      T.pack $
        "\nAfter intotoFExpr transformation:\n" ++ show result

  return result

-- Make intotoFExpr respect debug flag without using error
intotoFExpr :: Bool -> CPS.CExp -> FE.FExpr
intotoFExpr debugMode cexp =
  let result = case cexp of
        CPS.If c t f ->
          FE.If
            (intoAExpr debugMode c)
            (intotoFExpr debugMode t)
            (intotoFExpr debugMode f)
        CPS.SetThen n v c ->
          FE.SetThen
            n
            (intoAExpr debugMode v)
            (intotoFExpr debugMode c)
        CPS.Call1 f v ->
          FE.CallOne
            (intoAExpr debugMode f)
            (intoAExpr debugMode v)
        CPS.Call2 f v c ->
          FE.CallTwo
            (intoAExpr debugMode f)
            (intoAExpr debugMode v)
            (intoAExpr debugMode c)
   in result -- No debug logging in pure functions to avoid the error

intoAExpr :: Bool -> CPS.AExp -> FE.FExpr
intoAExpr debugMode aexp =
  let result = case aexp of
        CPS.Lam2 p k c ->
          FE.LamTwo p k (intotoFExpr debugMode c)
        CPS.Lam1 p c ->
          FE.LamOne p (intotoFExpr debugMode c)
        CPS.Var v ->
          FE.Var v
        CPS.BuiltinIdent s ->
          FE.BuiltinIdent s
        CPS.Lit l ->
          FE.Lit l
   in result -- No debug logging in pure functions to avoid the error

tC :: Bool -> BE.BExpr -> CPS.AExp -> Fresh CPS.CExp
tC debugMode expr c = do
  when debugMode $
    logDebugN $
      T.pack $
        "\ntC: Processing expression: "
          ++ show expr
          ++ "\nwith continuation: "
          ++ show c

  case expr of
    BE.Var v -> do
      when debugMode $
        logDebugN $
          T.pack $
            "tC: Variable reference: " ++ show v
      return $ CPS.Call1 c (CPS.Var v)
    BE.Lit lit -> do
      when debugMode $
        logDebugN $
          T.pack $
            "tC: Literal: " ++ show lit
      return $ CPS.Call1 c (CPS.Lit lit)
    BE.BuiltinIdent s -> do
      when debugMode $
        logDebugN $
          T.pack $
            "tC: Builtin identifier: " ++ show s
      return $ CPS.Call1 c (CPS.BuiltinIdent s)
    BE.Set n e -> do
      when debugMode $
        logDebugN $
          T.pack $
            "tC: Processing Set: " ++ show n ++ " = " ++ show e
      tK debugMode e $ \val -> do
        let result = CPS.SetThen n val (CPS.Call1 c (CPS.Lit LNil))
        when debugMode $
          logDebugN $
            T.pack $
              "tC: Set result: " ++ show result
        return result
    BE.If cond t f -> do
      when debugMode $
        logDebugN $
          T.pack $
            "tC: Processing If expression"
      k <- freshName "k"
      when debugMode $
        logDebugN $
          T.pack $
            "tC: Created fresh continuation for If: " ++ show k
      condExpr <- tK debugMode cond $ \v -> do
        t' <- tC debugMode t (CPS.Var k)
        f' <- tC debugMode f (CPS.Var k)
        let result = CPS.If v t' f'
        when debugMode $
          logDebugN $
            T.pack $
              "tC: If result: " ++ show result
        return result
      let result = CPS.Call1 (CPS.Lam1 k condExpr) c
      when debugMode $
        logDebugN $
          T.pack $
            "tC: Final If result: " ++ show result
      return result
    BE.Lam params body -> do
      when debugMode $
        logDebugN $
          T.pack $
            "tC: Processing Lambda: params=" ++ show params
      mexpr <- m debugMode expr
      let result = CPS.Call1 c mexpr
      when debugMode $
        logDebugN $
          T.pack $
            "tC: Lambda result: " ++ show result
      return result
    BE.App f a -> do
      when debugMode $
        logDebugN $
          T.pack $
            "tC: Processing Application: " ++ show f ++ " applied to " ++ show a

      when debugMode $
        logDebugN $
          T.pack $
            "tC: Using continuation: " ++ show c

      tK debugMode f $ \fval -> do
        when debugMode $
          logDebugN $
            T.pack $
              "tC: Function value: " ++ show fval

        tK debugMode a $ \aval -> do
          let result = CPS.Call2 fval aval c
          when debugMode $
            logDebugN $
              T.pack $
                "tC: Application result with direct continuation: " ++ show result
          return result

tK :: Bool -> BE.BExpr -> (CPS.AExp -> Fresh CPS.CExp) -> Fresh CPS.CExp
tK debugMode expr fk = do
  when debugMode $
    logDebugN $
      T.pack $
        "\ntK: Processing expression: " ++ show expr

  case expr of
    BE.Var v -> do
      when debugMode $
        logDebugN $
          T.pack $
            "tK: Variable reference: " ++ show v
      fk (CPS.Var v)
    BE.Lit lit -> do
      when debugMode $
        logDebugN $
          T.pack $
            "tK: Literal: " ++ show lit
      fk (CPS.Lit lit)
    BE.BuiltinIdent s -> do
      when debugMode $
        logDebugN $
          T.pack $
            "tK: Builtin: " ++ show s
      fk (CPS.BuiltinIdent s)
    BE.Lam _ _ -> do
      when debugMode $
        logDebugN $
          T.pack $
            "tK: Lambda"
      mexpr <- m debugMode expr
      fk mexpr
    BE.Set n e -> do
      when debugMode $
        logDebugN $
          T.pack $
            "tK: Setting variable: " ++ show n
      tK debugMode e $ \v -> do
        kexpr <- fk (CPS.Lit LNil)
        let result = CPS.SetThen n v kexpr
        when debugMode $
          logDebugN $
            T.pack $
              "tK: Set result: " ++ show result
        return result
    BE.If cond t f -> do
      when debugMode $
        logDebugN $
          T.pack $
            "tK: If expression"
      rv <- freshName "rv"
      kexpr <- fk (CPS.Var rv)
      tK debugMode cond $ \v -> do
        t' <- tC debugMode t (CPS.Lam1 rv kexpr)
        f' <- tC debugMode f (CPS.Lam1 rv kexpr)
        let result = CPS.If v t' f'
        when debugMode $
          logDebugN $
            T.pack $
              "tK: If result: " ++ show result
        return result
    BE.App f a -> do
      when debugMode $
        logDebugN $
          T.pack $
            "tK: Function application"
      rv <- freshName "rv"
      kexpr <- fk (CPS.Var rv)
      let cont = CPS.Lam1 rv kexpr
      when debugMode $
        logDebugN $
          T.pack $
            "tK: Created continuation: " ++ show cont
      tK debugMode f $ \fval -> do
        when debugMode $
          logDebugN $
            T.pack $
              "tK: Function value: " ++ show fval
        tK debugMode a $ \aval -> do
          let result = CPS.Call2 fval aval cont
          when debugMode $
            logDebugN $
              T.pack $
                "tK: Application result: " ++ show result
          return result

m :: Bool -> BE.BExpr -> Fresh CPS.AExp
m debugMode expr = do
  when debugMode $
    logDebugN $
      T.pack $
        "m: Processing expression: " ++ show expr

  case expr of
    BE.Var v -> do
      let result = CPS.Var v
      when debugMode $
        logDebugN $
          T.pack $
            "m: Created Var expression: " ++ show result
      return result
    BE.Lit l -> do
      let result = CPS.Lit l
      when debugMode $
        logDebugN $
          T.pack $
            "m: Created Lit expression: " ++ show result
      return result
    BE.BuiltinIdent s -> do
      let result = CPS.BuiltinIdent s
      when debugMode $
        logDebugN $
          T.pack $
            "m: Created BuiltinIdent expression: " ++ show result
      return result
    BE.Lam p e -> do
      k <- freshName "k"
      when debugMode $
        logDebugN $
          T.pack $
            "m: Created continuation name for lambda: " ++ show k
      body <- tC debugMode e (CPS.Var k)
      let result = CPS.Lam2 p k body
      when debugMode $
        logDebugN $
          T.pack $
            "m: Created lambda expression: " ++ show result
      return result
    _ -> error "m: invalid input"
