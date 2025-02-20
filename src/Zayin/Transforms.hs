{-# LANGUAGE OverloadedStrings #-}

module Zayin.Transforms (toFExprM) where

import Control.Monad.State
import qualified Data.Text as T
import Debug.Trace (trace)

import Zayin.AST (Fresh, Gen (..), freshName)
import qualified Zayin.BoundExpr as BE
import qualified Zayin.CPS as CPS
import qualified Zayin.FlatExpr as FE
import Zayin.Literals

toFExprM e k =
  trace ("\n=== Starting CPS Transformation ===\n" ++
         "Initial expression: " ++ show e ++ "\n" ++
         "Initial continuation: " ++ show k) $
  intotoFExpr <$> tC e k

intotoFExpr :: CPS.CExp -> FE.FExpr
intotoFExpr cexp =
  trace ("\nintotoFExpr: Processing expression: " ++ show cexp) $
  case cexp of
    CPS.If c ift iff ->
      let result = FE.If
            (intoAExpr c)
            (intotoFExpr ift)
            (intotoFExpr iff)
      in trace ("intotoFExpr: Created If expression: " ++ show result) result
    CPS.SetThen n v c ->
      let result = FE.SetThen n (intoAExpr v) (intotoFExpr c)
      in trace ("intotoFExpr: Created SetThen expression: " ++ show result) result
    CPS.Call1 f v ->
      let result = FE.CallOne (intoAExpr f) (intoAExpr v)
      in trace ("intotoFExpr: Created CallOne expression: " ++ show result) result
    CPS.Call2 f v c ->
      let result = FE.CallTwo (intoAExpr f) (intoAExpr v) (intoAExpr c)
      in trace ("intotoFExpr: Created CallTwo expression: " ++ show result) result

intoAExpr :: CPS.AExp -> FE.FExpr
intoAExpr aexp =
  trace ("\nintoAExpr: Processing expression: " ++ show aexp) $
  case aexp of
    CPS.Lam2 p k c ->
      let result = FE.LamTwo p k (intotoFExpr c)
      in trace ("intoAExpr: Created LamTwo expression: " ++ show result) result
    CPS.Lam1 p c ->
      let result = FE.LamOne p (intotoFExpr c)
      in trace ("intoAExpr: Created LamOne expression: " ++ show result) result
    CPS.Var v ->
      let result = FE.Var v
      in trace ("intoAExpr: Created Var expression: " ++ show result) result
    CPS.BuiltinIdent s ->
      let result = FE.BuiltinIdent s
      in trace ("intoAExpr: Created BuiltinIdent expression: " ++ show result) result
    CPS.Lit l ->
      let result = FE.Lit l
      in trace ("intoAExpr: Created Lit expression: " ++ show result) result

tC :: BE.BExpr -> CPS.AExp -> Fresh CPS.CExp
tC expr c =
  trace ("\ntC: Processing expression: " ++ show expr ++ "\n" ++
         "tC: With continuation: " ++ show c) $
  case expr of
    BE.Var v ->
      trace ("tC: Variable reference: " ++ show v) $
      return $ CPS.Call1 c (CPS.Var v)
    BE.Lit l ->
      trace ("tC: Literal: " ++ show l) $
      return $ CPS.Call1 c (CPS.Lit l)
    BE.BuiltinIdent s ->
      trace ("tC: Handling builtin: " ++ show s) $
      return $ CPS.Call1 c (CPS.BuiltinIdent s)
    BE.Set n e -> do
      trace ("tC: Setting variable: " ++ show n) $ pure ()
      let f v = return $ CPS.SetThen n v (CPS.Call1 c (CPS.Lit LNil))
      tK e f
    BE.If cond ift iff -> do
      trace "tC: Processing if expression" $ pure ()
      k <- freshName "k"
      trace ("tC: Created fresh continuation name: " ++ show k) $ pure ()
      tK_expr <- tK cond $ \v -> do
        trace "tC: Processing if condition result" $ pure ()
        t1 <- tC ift (CPS.Var k)
        t2 <- tC iff (CPS.Var k)
        return $ CPS.If v t1 t2
      return $ CPS.Call1 (CPS.Lam1 k tK_expr) c
    BE.Lam p e -> do
      trace ("tC: Processing lambda: params=" ++ show p) $ pure ()
      m_expr <- m expr
      return $ CPS.Call1 c m_expr
    BE.App f a -> do
      trace "tC: Processing application" $ pure ()
      rv <- freshName "rv"
      trace ("tC: Created new continuation: " ++ show rv) $ pure ()
      let cont = CPS.Lam1 rv (CPS.Call1 c (CPS.Var rv))
      result <- tK f $ \fv -> do
        trace ("tC: Function value: " ++ show fv) $ pure ()
        tK a $ \av -> do
          trace ("tC: Argument value: " ++ show av) $ pure ()
          trace "tC: Using new continuation" $ pure ()
          return $ CPS.Call2 fv av cont
      trace ("tC: Application result: " ++ show result) $ pure ()
      return result

tK :: BE.BExpr -> (CPS.AExp -> Fresh CPS.CExp) -> Fresh CPS.CExp
tK expr fk =
  trace ("\ntK: Processing expression: " ++ show expr) $
  case expr of
    BE.Var v ->
      trace ("tK: Variable reference: " ++ show v) $
      fk (CPS.Var v)
    BE.Lit l ->
      trace ("tK: Literal: " ++ show l) $
      fk (CPS.Lit l)
    BE.BuiltinIdent s ->
      trace ("tK: Builtin: " ++ show s) $
      fk (CPS.BuiltinIdent s)
    BE.Lam _ _ -> do
      trace "tK: Lambda" $ pure ()
      m expr >>= fk
    BE.Set n e -> do
      trace ("tK: Setting variable: " ++ show n) $ pure ()
      tK e $ \v -> do
        c <- fk (CPS.Lit LNil)
        return $ CPS.SetThen n v c
    BE.If cond ift iff -> do
      trace "tK: If expression" $ pure ()
      rv <- freshName "rv"
      cont <- fk (CPS.Var rv)
      tK cond $ \v -> do
        t1 <- tC ift (CPS.Lam1 rv cont)
        t2 <- tC iff (CPS.Lam1 rv cont)
        return $ CPS.If v t1 t2
    BE.App f a -> do
      trace "tK: Function application" $ pure ()
      rv <- freshName "rv"
      kResult <- fk (CPS.Var rv)
      let cont = CPS.Lam1 rv kResult
      trace ("tK: Created continuation: " ++ show cont) $ pure ()
      let fCont fv = tK a $ \av -> return $ CPS.Call2 fv av cont
      tK f fCont

m :: BE.BExpr -> Fresh CPS.AExp
m expr =
  trace ("m: Processing expression: " ++ show expr) $
  case expr of
    BE.Var v -> return $ CPS.Var v
    BE.Lit l -> return $ CPS.Lit l
    BE.BuiltinIdent s -> return $ CPS.BuiltinIdent s
    BE.Lam p e -> do
      k <- freshName "k"
      trace ("m: Created continuation name: " ++ show k) $ pure ()
      tC_expr <- tC e (CPS.Var k)
      return $ CPS.Lam2 p k tC_expr
    _ -> error "m: invalid input"
