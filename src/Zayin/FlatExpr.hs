{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Zayin.FlatExpr (FExpr (..), liftLambdas) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Debug.Trace (trace)


import qualified Zayin.LiftedExpr as L
import Zayin.Literals (Literal)

data FExpr
  = If FExpr FExpr FExpr
  | LamOne T.Text FExpr
  | LamTwo T.Text T.Text FExpr
  | Var T.Text
  | Lit Literal
  | BuiltinIdent T.Text
  | SetThen T.Text FExpr FExpr
  | CallOne FExpr FExpr
  | CallTwo FExpr FExpr FExpr
  deriving (Show, Eq)

data LiftingContext = LiftingContext
  { nextId :: Int,
    lambdas :: HashMap Int L.LiftedLambda
  }

initialContext :: LiftingContext
initialContext = LiftingContext 0 HashMap.empty

getFreshId :: LiftingContext -> (Int, LiftingContext)
getFreshId ctx = (nextId ctx, ctx {nextId = nextId ctx + 1})

addLambda :: L.LiftedLambda -> LiftingContext -> LiftingContext
addLambda lambda ctx = ctx {lambdas = HashMap.insert (L.lambdaId lambda) lambda (lambdas ctx)}

freeVars :: FExpr -> Set T.Text
freeVars expr = case expr of
  If c t f -> Set.unions [freeVars c, freeVars t, freeVars f]
  LamOne p e -> Set.delete p (freeVars e)
  LamTwo p1 p2 e -> Set.delete p1 (Set.delete p2 (freeVars e))
  Var v -> Set.singleton v
  Lit _ -> Set.empty
  BuiltinIdent _ -> Set.empty
  SetThen v e1 e2 -> Set.unions [Set.singleton v, freeVars e1, freeVars e2]
  CallOne f x -> Set.union (freeVars f) (freeVars x)
  CallTwo f x y -> Set.unions [freeVars f, freeVars x, freeVars y]

liftLambdasM :: FExpr -> LiftingContext -> (L.LExpr, LiftingContext)
liftLambdasM expr ctx =
  trace ("\n=== Lambda Lifting: Processing expression ===\n" ++
         "Expression: " ++ show expr ++ "\n" ++
         "Current context ID: " ++ show (nextId ctx)) $
  case expr of
    LamOne param body ->
      let free = freeVars body
          (id, ctx1) = getFreshId ctx
          (body', ctx2) = liftLambdasM body ctx1
          lambda = L.LiftedLambda id [param] free body'
          ctx3 = addLambda lambda ctx2
          result = (L.Lifted id, ctx3)
      in trace ("\nLifting LamOne: param=" ++ show param ++ "\n" ++
                "Free variables: " ++ show free ++ "\n" ++
                "Assigned lambda ID: " ++ show id ++ "\n" ++
                "Processed body: " ++ show body' ++ "\n" ++
                "Created lambda: " ++ show lambda)
         result

    LamTwo p1 p2 body ->
      let free = freeVars body
          (id, ctx1) = getFreshId ctx
          (body', ctx2) = liftLambdasM body ctx1
          lambda = L.LiftedLambda id [p1, p2] free body'
          ctx3 = addLambda lambda ctx2
          result = (L.Lifted id, ctx3)
      in trace ("\nLifting LamTwo: params=" ++ show [p1, p2] ++ "\n" ++
                "Free variables: " ++ show free ++ "\n" ++
                "Assigned lambda ID: " ++ show id ++ "\n" ++
                "Processed body: " ++ show body' ++ "\n" ++
                "Created lambda: " ++ show lambda)
         result

    If c t f ->
      let (c', ctx1) = liftLambdasM c ctx
          (t', ctx2) = liftLambdasM t ctx1
          (f', ctx3) = liftLambdasM f ctx2
          result = (L.If c' t' f', ctx3)
      in trace ("\nProcessing If expression\n" ++
                "Processed condition: " ++ show c' ++ "\n" ++
                "Processed then branch: " ++ show t' ++ "\n" ++
                "Processed else branch: " ++ show f' ++ "\n" ++
                "Final If expression: If " ++ show c' ++ " " ++ show t' ++ " " ++ show f')
         result

    SetThen v e1 e2 ->
      let (e1', ctx1) = liftLambdasM e1 ctx
          (e2', ctx2) = liftLambdasM e2 ctx1
          result = (L.SetThen v e1' e2', ctx2)
      in trace ("\nProcessing SetThen: var=" ++ show v ++ "\n" ++
                "Processed value expression: " ++ show e1' ++ "\n" ++
                "Processed continuation: " ++ show e2' ++ "\n" ++
                "Final SetThen: " ++ show v ++ " " ++ show e1' ++ " " ++ show e2')
         result

    CallOne f x -> case f of
      BuiltinIdent i ->
        let (x', ctx1) = liftLambdasM x ctx
            result = (L.CallOne (L.BuiltinIdent i) x', ctx1)
        in trace ("\nProcessing CallOne (Builtin)\n" ++
                  "Direct builtin: " ++ show i ++ "\n" ++
                  "Processed argument: " ++ show x' ++ "\n" ++
                  "Final CallOne: BuiltinIdent " ++ show i ++ " " ++ show x')
           result
      _ ->
        let (f', ctx1) = liftLambdasM f ctx
            (x', ctx2) = liftLambdasM x ctx1
            result = (L.CallOne f' x', ctx2)
        in trace ("\nProcessing CallOne\n" ++
                  "Processed function: " ++ show f' ++ "\n" ++
                  "Processed argument: " ++ show x' ++ "\n" ++
                  "Final CallOne: " ++ show f' ++ " " ++ show x')
           result

    CallTwo f x y -> case f of
      BuiltinIdent i ->
        let (x', ctx1) = liftLambdasM x ctx
            (y', ctx2) = liftLambdasM y ctx1
            result = (L.CallTwo (L.BuiltinIdent i) x' y', ctx2)
        in trace ("\nProcessing CallTwo (Builtin)\n" ++
                  "Direct builtin: " ++ show i ++ "\n" ++
                  "Processed first argument: " ++ show x' ++ "\n" ++
                  "Processed second argument: " ++ show y' ++ "\n" ++
                  "Final CallTwo: BuiltinIdent " ++ show i ++ " " ++ show x' ++ " " ++ show y')
           result
      _ ->
        let (f', ctx1) = liftLambdasM f ctx
            (x', ctx2) = liftLambdasM x ctx1
            (y', ctx3) = liftLambdasM y ctx2
            result = (L.CallTwo f' x' y', ctx3)
        in trace ("\nProcessing CallTwo\n" ++
                  "Processed function: " ++ show f' ++ "\n" ++
                  "Processed first argument: " ++ show x' ++ "\n" ++
                  "Processed second argument: " ++ show y' ++ "\n" ++
                  "Final CallTwo: " ++ show f' ++ " " ++ show x' ++ " " ++ show y')
           result

    Var v ->
      trace ("\nProcessing Var: " ++ show v)
      (L.Var v, ctx)

    Lit l ->
      trace ("\nProcessing Lit: " ++ show l)
      (L.Lit l, ctx)

    BuiltinIdent i ->
      trace ("\nProcessing BuiltinIdent: " ++ show i)
      (L.BuiltinIdent i, ctx)


liftLambdas :: FExpr -> (L.LExpr, HashMap Int L.LiftedLambda)
liftLambdas expr =
  let (lexpr, ctx) = liftLambdasM expr initialContext
   in (lexpr, lambdas ctx)
