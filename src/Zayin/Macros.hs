{-# LANGUAGE OverloadedStrings #-}

module Zayin.Macros (expandMacros) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Zayin.AST

-- A macro is represented by its parameter names and the template (an AST)
data Macro = Macro
  { macroParams :: [T.Text],
    macroTemplate :: Expr
  }
  deriving (Show, Eq)

type MacroEnv = Map.Map T.Text Macro

-- | substitute replaces every occurrence of the identifier `param`
-- in an expression with the substitution expression `arg`. This is a
-- simple, non-hygienic substitution.
substitute :: T.Text -> Expr -> Expr -> Expr
substitute param arg expr = case expr of
  EVar x ->
    if x == param then arg else EVar x
  ELit l -> ELit l
  EBuiltinIdent x -> EBuiltinIdent x
  EIf cond t f ->
    EIf
      (substitute param arg cond)
      (substitute param arg t)
      (substitute param arg f)
  ESet name e ->
    ESet name (substitute param arg e)
  ELet bindings body ->
    let newBindings =
          map
            ( \(n, e) ->
                (n, if n == param then e else substitute param arg e)
            )
            bindings
        newBody = substituteInBody param arg body
     in ELet newBindings newBody
  ELam ps body ->
    if param `elem` ps
      then ELam ps body -- do not substitute in a binding that shadows param
      else ELam ps (substituteInBody param arg body)
  EApp f args ->
    EApp (substitute param arg f) (map (substitute param arg) args)

substituteInBody :: T.Text -> Expr -> ExprBody -> ExprBody
substituteInBody param arg (ExprBody exprs final) =
  ExprBody (map subInExprBodyExpr exprs) (substitute param arg final)
  where
    subInExprBodyExpr (Def n e) =
      if n == param then Def n e else Def n (substitute param arg e)
    subInExprBodyExpr (Expr e) = Expr (substitute param arg e)

-- | applyMacro takes a macro definition and a list of argument ASTs.
-- It checks that the number of arguments matches and then performs
-- a sequential substitution of each macro parameter in the template.
applyMacro :: Macro -> [Expr] -> Either String Expr
applyMacro (Macro params template) args =
  if length params /= length args
    then Left "Macro argument count mismatch"
    else Right $ foldl (\acc (p, a) -> substitute p a acc) template (zip params args)

-- | extractMacro examines an expression that is expected to be a macro
-- definition and returns the macro’s name along with its transformation.
-- We expect a macro definition to be written as:
--
--    (macro (macroName) (lambda (…params…) template))
--
-- In our AST this looks like an ELam with a single parameter (the macro name)
-- whose body is an ELam representing the macro transformer.
extractMacro :: Expr -> Either String (T.Text, Macro)
extractMacro expr = case expr of
  ELam [mName] (ExprBody _ inner) ->
    case inner of
      ELam macroParams (ExprBody _ macroTemplate) ->
        Right (mName, Macro macroParams macroTemplate)
      _ -> Left "Invalid macro definition: inner lambda expected"
  _ -> Left "Invalid macro definition: expected lambda with one parameter"

-- | processBinding looks at a let-binding. If the binding’s name is "macro",
-- then we extract the macro (using extractMacro) and update the macro environment.
-- Otherwise, we simply expand the binding’s expression.
processBinding :: MacroEnv -> (T.Text, Expr) -> Either String (Maybe (T.Text, Expr), MacroEnv)
processBinding env (name, e)
  | name == "macro" =
      case extractMacro e of
        Right (macroName, macro) ->
          -- Register the macro under the provided name and remove the definition from the output.
          Right (Nothing, Map.insert macroName macro env)
        Left err -> Left $ "Error in macro definition: " ++ err
  | otherwise = do
      e' <- expandExpr env e
      return (Just (name, e'), env)

-- | processBindings handles a list of let-bindings,
-- updating the macro environment as needed.
processBindings :: MacroEnv -> [(T.Text, Expr)] -> Either String ([(T.Text, Expr)], MacroEnv)
processBindings env [] = Right ([], env)
processBindings env (b : bs) = do
  (maybeB, env') <- processBinding env b
  (rest, env'') <- processBindings env' bs
  case maybeB of
    Just binding -> Right (binding : rest, env'')
    Nothing -> Right (rest, env'')

-- | processExprBody traverses an ExprBody, expanding all expressions
-- and also processing any macro definitions within the body.
processExprBody :: MacroEnv -> ExprBody -> Either String (ExprBody, MacroEnv)
processExprBody env (ExprBody exprs final) = do
  (newExprs, env') <- processBodyExprs env exprs
  final' <- expandExpr env' final
  return (ExprBody newExprs final', env')

processBodyExprs :: MacroEnv -> [ExprBodyExpr] -> Either String ([ExprBodyExpr], MacroEnv)
processBodyExprs env [] = Right ([], env)
processBodyExprs env (x : xs) = do
  (maybeX, env') <- processBodyExpr env x
  (rest, env'') <- processBodyExprs env' xs
  case maybeX of
    Just exprBE -> Right (exprBE : rest, env'')
    Nothing -> Right (rest, env'')

-- | processBodyExpr processes a single expression inside an ExprBody.
-- If it is a macro definition (i.e. Def "macro" ...), we update the
-- environment and remove it from the final AST.
processBodyExpr :: MacroEnv -> ExprBodyExpr -> Either String (Maybe ExprBodyExpr, MacroEnv)
processBodyExpr env exprBody = case exprBody of
  Def name e
    | name == "macro" ->
        case extractMacro e of
          Right (macroName, macro) -> Right (Nothing, Map.insert macroName macro env)
          Left err -> Left $ "Error in macro definition: " ++ err
  Def name e -> do
    e' <- expandExpr env e
    return (Just (Def name e'), env)
  Expr e -> do
    e' <- expandExpr env e
    return (Just (Expr e'), env)

-- | expandExpr is the core recursive function. It traverses the AST,
-- updates the macro environment where necessary, and when it sees a
-- macro call, it applies the macro transformation.
expandExpr :: MacroEnv -> Expr -> Either String Expr
expandExpr env expr = case expr of
  EVar _ -> Right expr
  ELit _ -> Right expr
  EBuiltinIdent _ -> Right expr
  EIf cond t f -> do
    cond' <- expandExpr env cond
    t' <- expandExpr env t
    f' <- expandExpr env f
    Right $ EIf cond' t' f'
  ESet name e -> do
    e' <- expandExpr env e
    Right $ ESet name e'
  ELet bindings body -> do
    (newBindings, env') <- processBindings env bindings
    (body', env'') <- processExprBody env' body
    Right $ ELet newBindings body'
  ELam ps body -> do
    (body', env') <- processExprBody env body
    Right $ ELam ps body'
  EApp fun args -> do
    fun' <- expandExpr env fun
    -- If the function is a macro (i.e. an unadorned variable bound to a macro)
    case fun' of
      EVar macroName ->
        case Map.lookup macroName env of
          Just macro -> do
            -- Pass the raw (unevaluated) arguments to the macro transformer.
            expanded <- applyMacro macro args
            -- Recursively expand the result of the macro transformation.
            expandExpr env expanded
          Nothing -> do
            args' <- mapM (expandExpr env) args
            Right $ EApp fun' args'
      _ -> do
        fun'' <- expandExpr env fun'
        args' <- mapM (expandExpr env) args
        Right $ EApp fun'' args'

-- | Top-level entry point for macro expansion.
expandMacros :: Expr -> Either String Expr
expandMacros expr = expandExpr Map.empty expr
