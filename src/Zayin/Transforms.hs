{-# LANGUAGE OverloadedStrings #-}

module Zayin.Transforms (toFExprM) where

import Control.Monad.State
import qualified Data.Text as T
import Zayin.AST (Fresh, Gen (..), freshName)
import qualified Zayin.BoundExpr as BE
import qualified Zayin.CPS as CPS
import qualified Zayin.FlatExpr as FE
import Zayin.Literals

toFExprM :: BE.BExpr -> CPS.AExp -> Fresh FE.FExpr
toFExprM e k = intotoFExpr <$> tC e k

intotoFExpr :: CPS.CExp -> FE.FExpr
intotoFExpr cexp = case cexp of
  CPS.If c ift iff ->
    FE.If
      (intoAExpr c)
      (intotoFExpr ift)
      (intotoFExpr iff)
  CPS.SetThen n v c -> FE.SetThen n (intoAExpr v) (intotoFExpr c)
  CPS.Call1 f v -> FE.CallOne (intoAExpr f) (intoAExpr v)
  CPS.Call2 f v c -> FE.CallTwo (intoAExpr f) (intoAExpr v) (intoAExpr c)

intoAExpr :: CPS.AExp -> FE.FExpr
intoAExpr aexp = case aexp of
  CPS.Lam2 p k c -> FE.LamTwo p k (intotoFExpr c)
  CPS.Lam1 p c -> FE.LamOne p (intotoFExpr c)
  CPS.Var v -> FE.Var v
  CPS.BuiltinIdent s -> FE.BuiltinIdent s
  CPS.Lit l -> FE.Lit l

tC :: BE.BExpr -> CPS.AExp -> Fresh CPS.CExp
tC expr c = case expr of
  BE.Var v -> return $ CPS.Call1 c (CPS.Var v)
  BE.Lit l -> return $ CPS.Call1 c (CPS.Lit l)
  BE.BuiltinIdent s -> return $ CPS.Call1 c (CPS.BuiltinIdent s)
  BE.Set n e -> do
    let f v = return $ CPS.SetThen n v (CPS.Call1 c (CPS.Lit LNil))
    tK e f
  BE.If cond ift iff -> do
    k <- freshName "k"
    tK_expr <- tK cond $ \v -> do
      t1 <- tC ift (CPS.Var k)
      t2 <- tC iff (CPS.Var k)
      return $ CPS.If v t1 t2
    return $ CPS.Call1 (CPS.Lam1 k tK_expr) c
  BE.Lam p e -> do
    m_expr <- m expr
    return $ CPS.Call1 c m_expr
  BE.App f a -> do
    tK f $ \fv ->
      tK a $ \av ->
        return $ CPS.Call2 fv av c

tK :: BE.BExpr -> (CPS.AExp -> Fresh CPS.CExp) -> Fresh CPS.CExp
tK expr fk = case expr of
  BE.Var v -> fk (CPS.Var v)
  BE.Lit l -> fk (CPS.Lit l)
  BE.BuiltinIdent s -> fk (CPS.BuiltinIdent s)
  BE.Lam _ _ -> m expr >>= fk
  BE.Set n e -> tK e $ \v -> do
    c <- fk (CPS.Lit LNil)
    return $ CPS.SetThen n v c
  BE.If cond ift iff -> do
    rv <- freshName "rv"
    cont <- fk (CPS.Var rv)
    tK cond $ \v -> do
      t1 <- tC ift (CPS.Lam1 rv cont)
      t2 <- tC iff (CPS.Lam1 rv cont)
      return $ CPS.If v t1 t2
  BE.App f a -> do
    rv <- freshName "rv"
    kResult <- fk (CPS.Var rv)
    let cont = CPS.Lam1 rv kResult
    let fCont fv = tK a $ \av -> return $ CPS.Call2 fv av cont
    tK f fCont

m :: BE.BExpr -> Fresh CPS.AExp
m expr = case expr of
  BE.Var v -> return $ CPS.Var v
  BE.Lit l -> return $ CPS.Lit l
  BE.BuiltinIdent s -> return $ CPS.BuiltinIdent s
  BE.Lam p e -> do
    k <- freshName "k"
    tC_expr <- tC e (CPS.Var k)
    return $ CPS.Lam2 p k tC_expr
  _ -> error "m: invalid input"
