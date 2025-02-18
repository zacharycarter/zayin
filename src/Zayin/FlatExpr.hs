{-# LANGUAGE OverloadedStrings #-}

module Zayin.FlatExpr (FExpr (..), liftLambdas) where

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

liftLambdasM :: FExpr -> LiftingContext -> (L.LExpr, LiftingContext)
liftLambdasM expr ctx = case expr of
  LamOne param body ->
    let free = freeVars body
        (id, ctx1) = getFreshId ctx
        (body', ctx2) = liftLambdasM body ctx1
        lambda = L.LiftedLambda id [param] free body'
        ctx3 = addLambda lambda ctx2
     in (L.Lifted id, ctx3)
  LamTwo p1 p2 body ->
    let free = freeVars body
        (id, ctx1) = getFreshId ctx
        (body', ctx2) = liftLambdasM body ctx1
        lambda = L.LiftedLambda id [p1, p2] free body'
        ctx3 = addLambda lambda ctx2
     in (L.Lifted id, ctx3)
  If c t f ->
    let (c', ctx1) = liftLambdasM c ctx
        (t', ctx2) = liftLambdasM t ctx1
        (f', ctx3) = liftLambdasM f ctx2
     in (L.If c' t' f', ctx3)
  SetThen v e1 e2 ->
    let (e1', ctx1) = liftLambdasM e1 ctx
        (e2', ctx2) = liftLambdasM e2 ctx1
     in (L.SetThen v e1' e2', ctx2)
  CallOne f x ->
    let (f', ctx1) = liftLambdasM f ctx
        (x', ctx2) = liftLambdasM x ctx1
     in (L.CallOne f' x', ctx2)
  CallTwo f x y ->
    let (f', ctx1) = liftLambdasM f ctx
        (x', ctx2) = liftLambdasM x ctx1
        (y', ctx3) = liftLambdasM y ctx2
     in (L.CallTwo f' x' y', ctx3)
  Var v -> (L.Var v, ctx)
  Lit l -> (L.Lit l, ctx)
  BuiltinIdent i -> (L.BuiltinIdent i, ctx)

liftLambdas :: FExpr -> (L.LExpr, HashMap Int L.LiftedLambda)
liftLambdas expr =
  let (lexpr, ctx) = liftLambdasM expr initialContext
   in (lexpr, lambdas ctx)
