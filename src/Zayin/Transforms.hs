{-# LANGUAGE OverloadedStrings #-}

module Zayin.Transforms (toFExprM) where

import Control.Monad.State
import qualified Data.Text as T
import Debug.Trace (trace)
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import Control.Monad (foldM)

import Zayin.AST (Fresh, Gen (..), freshName)
import qualified Zayin.BoundExpr as BE
import qualified Zayin.CPS as CPS
import qualified Zayin.FlatExpr as FE
import Zayin.Literals

toFExprM :: BE.BExpr -> CPS.AExp -> Fresh FE.FExpr
toFExprM e k = do
    trace ("\n=== Starting CPS Transformation ===\n" ++
           "Initial bound expression: " ++ show e ++ "\n" ++
           "Initial continuation: " ++ show k) $ do
        c_expr <- tC e k
        trace ("\nAfter tC transformation:\n" ++ show c_expr) $ do
            let result = intotoFExpr c_expr
            trace ("\nAfter intotoFExpr transformation:\n" ++ show result) $
                return result

intotoFExpr :: CPS.CExp -> FE.FExpr
intotoFExpr cexp =
    trace ("\nintotoFExpr: Processing expression: " ++ show cexp) $
    case cexp of
        CPS.If c t f -> do
            let result = FE.If
                    (intoAExpr c)
                    (intotoFExpr t)
                    (intotoFExpr f)
            trace ("intotoFExpr: Created If expression: " ++ show result) result

        CPS.SetThen n v c -> do
            let result = FE.SetThen n
                    (intoAExpr v)
                    (intotoFExpr c)
            trace ("intotoFExpr: Created SetThen expression: " ++ show result) result

        CPS.Call1 f v -> do
            let result = FE.CallOne
                    (intoAExpr f)
                    (intoAExpr v)
            trace ("intotoFExpr: Created CallOne expression: " ++ show result) result

        CPS.Call2 f v c -> do
            let result = FE.CallTwo
                    (intoAExpr f)
                    (intoAExpr v)
                    (intoAExpr c)
            trace ("intotoFExpr: Created CallTwo expression: " ++ show result) result

intoAExpr :: CPS.AExp -> FE.FExpr
intoAExpr aexp =
    trace ("\nintoAExpr: Processing expression: " ++ show aexp) $
    case aexp of
        CPS.Lam2 p k c -> do
            let result = FE.LamTwo p k (intotoFExpr c)
            trace ("intoAExpr: Created LamTwo expression: " ++ show result) result

        CPS.Lam1 p c -> do
            let result = FE.LamOne p (intotoFExpr c)
            trace ("intoAExpr: Created LamOne expression: " ++ show result) result

        CPS.Var v -> do
            let result = FE.Var v
            trace ("intoAExpr: Created Var expression: " ++ show result) result

        CPS.BuiltinIdent s -> do
            let result = FE.BuiltinIdent s
            trace ("intoAExpr: Created BuiltinIdent expression: " ++ show result) result

        CPS.Lit l -> do
            let result = FE.Lit l
            trace ("intoAExpr: Created Lit expression: " ++ show result) result


tC :: BE.BExpr -> CPS.AExp -> Fresh CPS.CExp
tC expr c = do
    trace ("\ntC: Processing expression: " ++ show expr ++
           "\nwith continuation: " ++ show c) $
      case expr of
        BE.Var v -> do
            trace ("tC: Variable reference: " ++ show v) $
                return $ CPS.Call1 c (CPS.Var v)

        BE.Lit lit -> do
            trace ("tC: Literal: " ++ show lit) $
                return $ CPS.Call1 c (CPS.Lit lit)

        BE.BuiltinIdent s -> do
            trace ("tC: Builtin identifier: " ++ show s) $
                return $ CPS.Call1 c (CPS.BuiltinIdent s)

        BE.Set n e -> do
            trace ("tC: Processing Set: " ++ show n ++ " = " ++ show e) $ do
                v <- freshName "tmp"
                trace ("tC: Created fresh name for Set: " ++ show v) $
                    tK e $ \val -> do
                        let result = CPS.SetThen n val (CPS.Call1 c (CPS.Lit LNil))
                        trace ("tC: Set result: " ++ show result) $
                            return result

        BE.If cond t f -> do
            trace "tC: Processing If expression" $ do
                k <- freshName "k"
                trace ("tC: Created fresh continuation for If: " ++ show k) $ do
                    condExpr <- tK cond $ \v -> do
                        t' <- tC t (CPS.Var k)
                        f' <- tC f (CPS.Var k)
                        let result = CPS.If v t' f'
                        trace ("tC: If result: " ++ show result) $
                            return result
                    let result = CPS.Call1 (CPS.Lam1 k condExpr) c
                    trace ("tC: Final If result: " ++ show result) $
                        return result

        BE.Lam params body -> do
            trace ("tC: Processing Lambda: params=" ++ show params) $ do
                mexpr <- m expr
                let result = CPS.Call1 c mexpr
                trace ("tC: Lambda result: " ++ show result) $
                    return result

        BE.App f a -> do
            trace ("tC: Processing Application: " ++ show f ++ " applied to " ++ show a) $ do
                trace ("tC: Using continuation: " ++ show c) $ do
                    tK f $ \fval -> do
                        trace ("tC: Function value: " ++ show fval) $
                            tK a $ \aval -> do
                                let result = CPS.Call2 fval aval c
                                trace ("tC: Application result with direct continuation: " ++ show result) $
                                    return result

tK :: BE.BExpr -> (CPS.AExp -> Fresh CPS.CExp) -> Fresh CPS.CExp
tK expr fk = do
    trace ("\ntK: Processing expression: " ++ show expr) $
      case expr of
        BE.Var v -> do
            trace ("tK: Variable reference: " ++ show v) $
                fk (CPS.Var v)

        BE.Lit lit -> do
            trace ("tK: Literal: " ++ show lit) $
                fk (CPS.Lit lit)

        BE.BuiltinIdent s -> do
            trace ("tK: Builtin: " ++ show s) $
                fk (CPS.BuiltinIdent s)

        BE.Lam _ _ -> do
            trace "tK: Lambda" $ do
                mexpr <- m expr
                fk mexpr

        BE.Set n e -> do
            trace ("tK: Setting variable: " ++ show n) $
                tK e $ \v -> do
                    kexpr <- fk (CPS.Lit LNil)
                    let result = CPS.SetThen n v kexpr
                    trace ("tK: Set result: " ++ show result) $
                        return result

        BE.If cond t f -> do
            trace "tK: If expression" $ do
                rv <- freshName "rv"
                kexpr <- fk (CPS.Var rv)
                tK cond $ \v -> do
                    t' <- tC t (CPS.Lam1 rv kexpr)
                    f' <- tC f (CPS.Lam1 rv kexpr)
                    let result = CPS.If v t' f'
                    trace ("tK: If result: " ++ show result) $
                        return result

        BE.App f a -> do
            trace "tK: Function application" $ do
                rv <- freshName "rv"
                kexpr <- fk (CPS.Var rv)
                let cont = CPS.Lam1 rv kexpr
                trace ("tK: Created continuation: " ++ show cont) $
                    tK f $ \fval -> do
                        trace ("tK: Function value: " ++ show fval) $
                            tK a $ \aval -> do
                                let result = CPS.Call2 fval aval cont
                                trace ("tK: Application result: " ++ show result) $
                                    return result

m :: BE.BExpr -> Fresh CPS.AExp
m expr = do
    trace ("m: Processing expression: " ++ show expr) $
        case expr of
            BE.Var v -> do
                let result = CPS.Var v
                trace ("m: Created Var expression: " ++ show result) $
                    return result
            BE.Lit l -> do
                let result = CPS.Lit l
                trace ("m: Created Lit expression: " ++ show result) $
                    return result
            BE.BuiltinIdent s -> do
                let result = CPS.BuiltinIdent s
                trace ("m: Created BuiltinIdent expression: " ++ show result) $
                    return result
            BE.Lam p e -> do
                k <- freshName "k"
                trace ("m: Created continuation name for lambda: " ++ show k) $ do
                    body <- tC e (CPS.Var k)
                    let result = CPS.Lam2 p k body
                    trace ("m: Created lambda expression: " ++ show result) $
                        return result
            _ -> error "m: invalid input"
