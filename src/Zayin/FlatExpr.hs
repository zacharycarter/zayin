{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Zayin.FlatExpr (FExpr (..), liftLambdas) where

import Debug.Trace (trace)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
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

freeVarsLExpr :: HashMap Int L.LiftedLambda -> L.LExpr -> Set T.Text
freeVarsLExpr lambdaMap expr = case expr of
  L.If c t f ->
    freeVarsLExpr lambdaMap c `Set.union`
    freeVarsLExpr lambdaMap t `Set.union`
    freeVarsLExpr lambdaMap f
  L.Var v -> Set.singleton v
  L.Lit _ -> Set.empty
  L.BuiltinIdent _ -> Set.empty
  L.SetThen v e1 e2 ->
    Set.unions [Set.singleton v, freeVarsLExpr lambdaMap e1, freeVarsLExpr lambdaMap e2]
  L.CallOne f x ->
    freeVarsLExpr lambdaMap f `Set.union` freeVarsLExpr lambdaMap x
  L.CallTwo f x y ->
    freeVarsLExpr lambdaMap f `Set.union`
    freeVarsLExpr lambdaMap x `Set.union`
    freeVarsLExpr lambdaMap y
  L.Lifted i ->
    case HashMap.lookup i lambdaMap of
      Just liftedLambda -> L.freeVars liftedLambda  -- use the field from the lifted lambda record
      Nothing -> Set.empty

-- Use trace for debugging, which is safe in pure code
debugTrace :: Bool -> String -> a -> a
debugTrace False _ x = x
debugTrace True msg x = trace msg x

liftLambdasM :: Bool -> FExpr -> LiftingContext -> (L.LExpr, LiftingContext)
liftLambdasM debugMode expr ctx =
  debugTrace debugMode
    ( "\n=== Lambda Lifting: Processing expression ===\n"
        ++ "Expression: "
        ++ show expr
        ++ "\n"
        ++ "Current context ID: "
        ++ show (nextId ctx)
    )
    $ case expr of
      LamOne param body ->
        let (body', ctx1) = liftLambdasM debugMode body ctx
            free = freeVarsLExpr (lambdas ctx1) body' -- Compute free vars AFTER lifting nested lambdas
            (id, ctx2) = getFreshId ctx1
            lambda = L.LiftedLambda id [param] free body'
            ctx3 = addLambda lambda ctx2
            result = (L.Lifted id, ctx3)
         in debugTrace debugMode
              ( "\nLifting LamOne: param="
                  ++ show param
                  ++ "\n"
                  ++ "Free variables: "
                  ++ show free
                  ++ "\n"
                  ++ "Assigned lambda ID: "
                  ++ show id
                  ++ "\n"
                  ++ "Processed body: "
                  ++ show body'
                  ++ "\n"
                  ++ "Created lambda: "
                  ++ show lambda
              )
              result
      LamTwo p1 p2 body ->
        let (body', ctx1) = liftLambdasM debugMode body ctx
            free = freeVarsLExpr (lambdas ctx1) body' -- Compute free vars AFTER lifting nested lambdas
            (id, ctx2) = getFreshId ctx1
            lambda = L.LiftedLambda id [p1, p2] free body'
            ctx3 = addLambda lambda ctx2
            result = (L.Lifted id, ctx3)
         in debugTrace debugMode
              ( "\nLifting LamTwo: params="
                  ++ show [p1, p2]
                  ++ "\n"
                  ++ "Free variables: "
                  ++ show free
                  ++ "\n"
                  ++ "Assigned lambda ID: "
                  ++ show id
                  ++ "\n"
                  ++ "Processed body: "
                  ++ show body'
                  ++ "\n"
                  ++ "Created lambda: "
                  ++ show lambda
              )
              result
      If c t f ->
        let (c', ctx1) = liftLambdasM debugMode c ctx
            (t', ctx2) = liftLambdasM debugMode t ctx1
            (f', ctx3) = liftLambdasM debugMode f ctx2
            result = (L.If c' t' f', ctx3)
         in debugTrace debugMode
              ( "\nProcessing If expression\n"
                  ++ "Processed condition: "
                  ++ show c'
                  ++ "\n"
                  ++ "Processed then branch: "
                  ++ show t'
                  ++ "\n"
                  ++ "Processed else branch: "
                  ++ show f'
                  ++ "\n"
                  ++ "Final If expression: If "
                  ++ show c'
                  ++ " "
                  ++ show t'
                  ++ " "
                  ++ show f'
              )
              result
      SetThen v e1 e2 ->
        let (e1', ctx1) = liftLambdasM debugMode e1 ctx
            (e2', ctx2) = liftLambdasM debugMode e2 ctx1
            result = (L.SetThen v e1' e2', ctx2)
         in debugTrace debugMode
              ( "\nProcessing SetThen: var="
                  ++ show v
                  ++ "\n"
                  ++ "Processed value expression: "
                  ++ show e1'
                  ++ "\n"
                  ++ "Processed continuation: "
                  ++ show e2'
                  ++ "\n"
                  ++ "Final SetThen: "
                  ++ show v
                  ++ " "
                  ++ show e1'
                  ++ " "
                  ++ show e2'
              )
              result
      CallOne f x -> case f of
        BuiltinIdent i ->
          let (x', ctx1) = liftLambdasM debugMode x ctx
              result = (L.CallOne (L.BuiltinIdent i) x', ctx1)
           in debugTrace debugMode
                ( "\nProcessing CallOne (Builtin)\n"
                    ++ "Direct builtin: "
                    ++ show i
                    ++ "\n"
                    ++ "Processed argument: "
                    ++ show x'
                    ++ "\n"
                    ++ "Final CallOne: BuiltinIdent "
                    ++ show i
                    ++ " "
                    ++ show x'
                )
                result
        _ ->
          let (f', ctx1) = liftLambdasM debugMode f ctx
              (x', ctx2) = liftLambdasM debugMode x ctx1
              result = (L.CallOne f' x', ctx2)
           in debugTrace debugMode
                ( "\nProcessing CallOne\n"
                    ++ "Processed function: "
                    ++ show f'
                    ++ "\n"
                    ++ "Processed argument: "
                    ++ show x'
                    ++ "\n"
                    ++ "Final CallOne: "
                    ++ show f'
                    ++ " "
                    ++ show x'
                )
                result
      CallTwo f x y -> case f of
        BuiltinIdent i ->
          let (x', ctx1) = liftLambdasM debugMode x ctx
              (y', ctx2) = liftLambdasM debugMode y ctx1
              result = (L.CallTwo (L.BuiltinIdent i) x' y', ctx2)
           in debugTrace debugMode
                ( "\nProcessing CallTwo (Builtin)\n"
                    ++ "Direct builtin: "
                    ++ show i
                    ++ "\n"
                    ++ "Processed first argument: "
                    ++ show x'
                    ++ "\n"
                    ++ "Processed second argument: "
                    ++ show y'
                    ++ "\n"
                    ++ "Final CallTwo: BuiltinIdent "
                    ++ show i
                    ++ " "
                    ++ show x'
                    ++ " "
                    ++ show y'
                    ++ "\n"
                )
                result
        _ ->
          let (f', ctx1) = liftLambdasM debugMode f ctx
              (x', ctx2) = liftLambdasM debugMode x ctx1
              (y', ctx3) = liftLambdasM debugMode y ctx2
              result = (L.CallTwo f' x' y', ctx3)
           in debugTrace debugMode
                ( "\nProcessing CallTwo\n"
                    ++ "Processed function: "
                    ++ show f'
                    ++ "\n"
                    ++ "Processed first argument: "
                    ++ show x'
                    ++ "\n"
                    ++ "Processed second argument: "
                    ++ show y'
                    ++ "\n"
                    ++ "Final CallTwo: "
                    ++ show f'
                    ++ " "
                    ++ show x'
                    ++ " "
                    ++ show y'
                    ++ "\n"
                )
                result
      Var v ->
        debugTrace debugMode
          ("\nProcessing Var: " ++ show v)
          (L.Var v, ctx)
      Lit l ->
        debugTrace debugMode
          ("\nProcessing Lit: " ++ show l)
          (L.Lit l, ctx)
      BuiltinIdent i ->
        debugTrace debugMode
          ("\nProcessing BuiltinIdent: " ++ show i)
          (L.BuiltinIdent i, ctx)

liftLambdas :: Bool -> FExpr -> (L.LExpr, HashMap Int L.LiftedLambda)
liftLambdas debugMode expr =
  let (lexpr, ctx) = liftLambdasM debugMode expr initialContext
   in (lexpr, lambdas ctx)
