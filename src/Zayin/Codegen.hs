{-# LANGUAGE OverloadedStrings #-}

module Zayin.Codegen
  ( CDecl (..),
    CExpr (..),
    CStmt (..),
    CType (..),
    codegen,
  )
where

import Control.Monad (forM)
import Control.Monad.State
import Data.Foldable (traverse_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Zayin.LiftedExpr
import Zayin.Literals

data CType
  = TVoid
  | TPtr CType
  | TStruct T.Text
  deriving (Show, Eq)

data CExpr
  = EVar T.Text
  | ELitInt Integer
  | ELitStr T.Text
  | EBinOp T.Text CExpr CExpr
  | EPreUnOp T.Text CExpr
  | EArrow CExpr T.Text
  | ECast CExpr CType
  | EMacroCall T.Text [CExpr]
  | ENull
  deriving (Show, Eq)

data CStmt
  = SExpr CExpr
  | SDecl CDecl
  | SIf CExpr CStmt CStmt
  | SBlock [CStmt]
  deriving (Show, Eq)

data CDecl
  = DStruct
      { dName :: T.Text,
        dMembers :: [(T.Text, CType)]
      }
  | DFunProto
      { dName :: T.Text,
        dRetType :: CType,
        dParams :: [(T.Text, CType)],
        dNoreturn :: Bool
      }
  | DFun
      { dName :: T.Text,
        dRetType :: CType,
        dParams :: [(T.Text, CType)],
        dBody :: [CStmt]
      }
  | DVar
      { dName :: T.Text,
        dType :: CType,
        dInit :: Maybe CExpr
      }
  deriving (Show, Eq)

data CodegenCtx = CodegenCtx
  { ctxVarId :: Int,
    ctxProtos :: [CDecl],
    ctxDecls :: [CDecl],
    ctxLambdas :: HashMap Int LiftedLambda
  }

initialCtx :: HashMap Int LiftedLambda -> CodegenCtx
initialCtx lambdas = CodegenCtx 0 [] [] lambdas

type Codegen a = State CodegenCtx a

genVarId :: Codegen Int
genVarId = do
  ctx <- get
  let varId = ctxVarId ctx
  put ctx {ctxVarId = varId + 1}
  return varId

genVar :: Codegen T.Text
genVar = do
  varId <- genVarId
  return $ "var_" <> T.pack (show varId)

addProto :: CDecl -> Codegen ()
addProto proto = modify $ \ctx ->
  ctx {ctxProtos = proto : ctxProtos ctx}

addDecl :: CDecl -> Codegen ()
addDecl decl = modify $ \ctx ->
  ctx {ctxDecls = decl : ctxDecls ctx}

nameForFreeVar :: T.Text -> T.Text
nameForFreeVar name =
  let sanitized = T.filter (\c -> c `elem` ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']) name
   in "v_" <> sanitized

objectType :: CType
objectType = TPtr (TStruct "obj")

codegen :: LExpr -> HashMap Int LiftedLambda -> ([CStmt], [CDecl], [CDecl])
codegen expr lambdas =
  let ctx = initialCtx lambdas
      ((finalStmts, finalExpr), finalCtx) =
        runState
          ( do
              -- Generate protos and function bodies for all lambdas
              traverse_
                ( \l -> do
                    addProto (envStruct l)
                    generateFunc l
                )
                (HashMap.elems lambdas)

              -- Generate the main expression
              doCodegen expr
          )
          ctx
   in ( finalStmts ++ [SExpr finalExpr],
        reverse $ ctxProtos finalCtx,
        reverse $ ctxDecls finalCtx
      )

envStruct :: LiftedLambda -> CDecl
envStruct lambda =
  DStruct
    { dName = "env_" <> T.pack (show $ lambdaId lambda),
      dMembers = map (\v -> (nameForFreeVar v, objectType)) (Set.toList $ freeVars lambda)
    }

constructEnvCode :: LiftedLambda -> T.Text -> CStmt
constructEnvCode lambda dest =
  SExpr $
    EMacroCall
      "OBJECT_ENV_OBJ_NEW"
      [ EVar dest,
        EVar $ "struct env_" <> T.pack (show $ lambdaId lambda)
      ]

makeEnvCode :: LiftedLambda -> CExpr -> Codegen (CExpr, [CStmt])
makeEnvCode lambda srcEnv = do
  varName <- genVar
  let envExpr = EVar varName
      envAccess = generateEnvCast lambda envExpr

  let copyVars = filter (\v -> not $ v `elem` params lambda) (Set.toList $ freeVars lambda)
      copyStmts =
        map
          ( \v ->
              SExpr $
                EBinOp
                  "="
                  (EArrow envAccess (nameForFreeVar v))
                  (EArrow srcEnv (nameForFreeVar v))
          )
          copyVars

  return (envExpr, constructEnvCode lambda varName : copyStmts)

generateClosure :: LiftedLambda -> CExpr -> Codegen (CExpr, [CStmt])
generateClosure lambda currentEnv = do
  (envExpr, envStmts) <- makeEnvCode lambda currentEnv

  let initName = case length (params lambda) of
        1 -> "OBJECT_CLOSURE_ONE_NEW"
        2 -> "OBJECT_CLOSURE_TWO_NEW"
        n -> error $ "closure had " ++ show n ++ " parameters"

  varName <- genVar
  let initStmt =
        SExpr $
          EMacroCall
            initName
            [ EVar varName,
              EVar $ "lambda_" <> T.pack (show $ lambdaId lambda),
              envExpr
            ]

  return (EVar varName, envStmts ++ [initStmt])

generateEnvCast :: LiftedLambda -> CExpr -> CExpr
generateEnvCast lambda expr =
  ECast
    (EPreUnOp "&" (EArrow expr "env"))
    (TPtr $ TStruct $ "env_" <> T.pack (show $ lambdaId lambda))

generateFunc :: LiftedLambda -> Codegen ()
generateFunc lambda = do
  let paramNames = map (\p -> ("param_" <> T.pack (show p), objectType)) [0 .. length (params lambda) - 1]
      envObjType = TPtr (TStruct "env_obj")
      allParams = paramNames ++ [("env_in", envObjType)]

  -- Add function prototype
  addProto
    DFunProto
      { dName = "lambda_" <> T.pack (show $ lambdaId lambda),
        dRetType = TVoid,
        dParams = allParams,
        dNoreturn = True
      }

  -- Generate function body
  let envMove =
        SDecl
          DVar
            { dName = "env",
              dType = TPtr $ TStruct $ "env_" <> T.pack (show $ lambdaId lambda),
              dInit = Just $ generateEnvCast lambda (EVar "env_in")
            }

  -- Copy parameters to environment
  paramMoves <- forM (zip (params lambda) paramNames) $ \(param, (pName, _)) ->
   if param `elem` Set.toList (freeVars lambda)
      then do
        tmpName <- genVar
        let moves =
              [ SExpr $ EMacroCall "OBJECT_CELL_OBJ_NEW" [EVar tmpName, EVar pName],
                SExpr $ EBinOp "=" (EArrow (EVar "env") (nameForFreeVar param)) (EVar tmpName)
              ]
        return moves
      else return []

  -- Generate the body expression
  (bodyStmts, bodyExpr) <- doCodegen (body lambda)

  let allStmts =
        envMove
          : concat paramMoves
          ++ bodyStmts
          ++ [ SExpr bodyExpr,
               SExpr $ EMacroCall "__builtin_unreachable" []
             ]

  addDecl
    DFun
      { dName = "lambda_" <> T.pack (show $ lambdaId lambda),
        dRetType = TVoid,
        dParams = allParams,
        dBody = allStmts
      }

builtinIdentCodegen :: T.Text -> Codegen ([CStmt], CExpr)
builtinIdentCodegen ident = do
  let (numParams, runtimeName) = case ident of
        "tostring" -> (2, "to_string_k")
        "display" -> (2, "display_k")
        "exit" -> (1, "exit_k")
        "+" -> (2, "add_k")
        "-" -> (2, "sub_k")
        "*" -> (2, "mul_k")
        "/" -> (2, "div_k")
        "%" -> (2, "mod_k")
        "^" -> (2, "xor_k")
        "<" -> (2, "lt_k")
        "<=" -> (2, "leq_k")
        ">" -> (2, "gt_k")
        ">=" -> (2, "geq_k")
        "cons" -> (2, "cons_k")
        "cons?" -> (2, "is_cons_k")
        "null?" -> (2, "is_null_k")
        "car" -> (2, "car_k")
        "cdr" -> (2, "cdr_k")
        "string-concat" -> (2, "string_concat_k")
        "string-chars" -> (2, "string_chars_k")
        "ht-new" -> (2, "ht_new_k")
        "ht-set!" -> (2, "ht_set_k")
        "ht-get" -> (2, "ht_get_k")
        "ht-del!" -> (2, "ht_del_k")
        "ht-keys" -> (2, "ht_keys_k")
        "eq?" -> (2, "eq_k")
        _ -> error $ "unknown builtin: " ++ T.unpack ident

  let initName = case numParams of
        1 -> "OBJECT_CLOSURE_ONE_NEW"
        2 -> "OBJECT_CLOSURE_TWO_NEW"
        n -> error $ "closure had " ++ show n ++ " parameters"

  varName <- genVar
  let initStmt =
        SExpr $
          EMacroCall
            initName
            [ EVar varName,
              EVar runtimeName,
              ENull
            ]

  return ([initStmt], EVar varName)

doCodegen :: LExpr -> Codegen ([CStmt], CExpr)
doCodegen expr = case expr of
  Var name ->
    return
      ( [],
        EArrow
          ( ECast
              (EArrow (EVar "env") (nameForFreeVar name))
              (TPtr $ TStruct "cell_obj")
          )
          "val"
      )
  Lit lit -> case lit of
    LString s -> do
      dest <- genVar
      let init =
            SExpr $
              EMacroCall
                "OBJECT_STRING_OBJ_NEW"
                [EVar dest, ELitStr s]
      return ([init], EVar dest)
    LInt i -> do
      dest <- genVar
      let init =
            SExpr $
              EMacroCall
                "OBJECT_INT_OBJ_NEW"
                [EVar dest, ELitInt i]
      return ([init], EVar dest)
    LNil -> return ([], ENull)
    _ -> error "unsupported literal type"
  BuiltinIdent ident -> builtinIdentCodegen ident
  SetThen var val cont -> do
    (valStmts, valExpr) <- doCodegen val
    let resolvedName = nameForFreeVar var
        setStmt =
          SExpr $
            EBinOp
              "="
              ( EArrow
                  ( ECast
                      (EArrow (EVar "env") resolvedName)
                      (TPtr $ TStruct "cell_obj")
                  )
                  "val"
              )
              valExpr
    (contStmts, contExpr) <- doCodegen cont
    return (valStmts ++ [setStmt] ++ contStmts, contExpr)
  Lifted id -> do
    ctx <- get
    let lambda = HashMap.lookup id (ctxLambdas ctx)
    case lambda of
      Just l -> do
        (closureExpr, stmts) <- generateClosure l (EVar "env")
        return (stmts, closureExpr)
      Nothing -> error $ "unknown lambda id: " ++ show id
  If cond thenExpr elseExpr -> do
    (condStmts, condExpr) <- doCodegen cond
    (thenStmts, thenFinal) <- doCodegen thenExpr
    (elseStmts, elseFinal) <- doCodegen elseExpr

    let ifStmt =
          SIf
            (EMacroCall "obj_is_truthy" [condExpr])
            (SBlock $ thenStmts ++ [SExpr thenFinal])
            (SBlock $ elseStmts ++ [SExpr elseFinal])

    return (condStmts ++ [ifStmt], ELitInt 0)
  CallOne f arg -> do
    (fStmts, fExpr) <- doCodegen f
    (argStmts, argExpr) <- doCodegen arg
    return
      ( fStmts ++ argStmts,
        EMacroCall "call_closure_one" [fExpr, argExpr]
      )
  CallTwo f arg1 arg2 -> do
    (fStmts, fExpr) <- doCodegen f
    (arg1Stmts, arg1Expr) <- doCodegen arg1
    (arg2Stmts, arg2Expr) <- doCodegen arg2
    return
      ( fStmts ++ arg1Stmts ++ arg2Stmts,
        EMacroCall "call_closure_two" [fExpr, arg1Expr, arg2Expr]
      )
