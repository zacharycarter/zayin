{-# LANGUAGE OverloadedStrings #-}

module Zayin.Codegen
  ( CDecl (..),
    CExpr (..),
    CStmt (..),
    CType (..),
    codegen,
  )
where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.State
import Data.Char (isAlphaNum)
import Data.Foldable (traverse_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Debug.Trace (traceShow)
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
    ctxStmts :: [CStmt],
    ctxLambdas :: HashMap Int LiftedLambda,
    ctxLambdaEnvs :: HashMap Int T.Text
  }

initialCtx :: HashMap Int LiftedLambda -> CodegenCtx
initialCtx lambdas = CodegenCtx 0 [] [] [] lambdas HashMap.empty

type Codegen a = LoggingT (StateT CodegenCtx IO) a

genVarId :: Codegen Int
genVarId = do
  ctx <- lift get
  let varId = ctxVarId ctx
  put ctx {ctxVarId = varId + 1}
  return varId

genVar :: Codegen T.Text
genVar = do
  varId <- genVarId
  return $ "var_" <> T.pack (show varId)

addProto :: CDecl -> Codegen ()
addProto proto = lift $ modify $ \ctx ->
  ctx {ctxProtos = proto : ctxProtos ctx}

addDecl :: CDecl -> Codegen ()
addDecl decl = lift $ modify $ \ctx ->
  ctx {ctxDecls = decl : ctxDecls ctx}

addStmt :: CStmt -> Codegen ()
addStmt stmt = lift $ modify $ \ctx ->
  ctx {ctxStmts = stmt : ctxStmts ctx}

clearStmts :: Codegen ()
clearStmts = lift $ modify $ \ctx ->
  ctx {ctxStmts = []}

nameForFreeVar :: T.Text -> T.Text
nameForFreeVar name =
  let sanitized = T.map (\c -> if isAlphaNum c then c else '_') name
   in "v_" <> sanitized

withStmtScope :: Codegen a -> Codegen (a, [CStmt])
withStmtScope action = do
  logDebugN "=== withStmtScope ==="
  -- Save current statements
  oldStmts <- gets ctxStmts
  logDebugN $ "oldStmts: " <> T.pack (show oldStmts)
  -- Clear statement buffer
  lift $ modify $ \ctx -> ctx {ctxStmts = []}
  clearedStmts <- gets ctxStmts
  logDebugN $ "cleared ctx: " <> T.pack (show clearedStmts)
  -- Run the action
  result <- action

  -- Get the new statements
  newStmts <- gets ctxStmts
  logDebugN $ "newStmts " <> T.pack (show newStmts)
  -- Restore old statements
  lift $ modify $ \ctx -> ctx {ctxStmts = oldStmts}
  restoredStmts <- gets ctxStmts
  logDebugN $ "restored ctx: " <> T.pack (show restoredStmts)
  -- Return result and statements from this scope
  return (result, reverse newStmts)

objectType :: CType
objectType = TPtr (TStruct "obj")

envStruct :: LiftedLambda -> Codegen CDecl
envStruct lambda = do
  let captured = Set.fromList (filter (not . T.isPrefixOf "_unused") (params lambda))
      unionFree = Set.union (freeVars lambda) captured
      members = map (\v -> (nameForFreeVar v, objectType)) (Set.toList unionFree)
      filtered = filter (\(name, _) -> name /= "v_") members
  logDebugN $
    "envStruct for lambda "
      <> T.pack (show (lambdaId lambda))
      <> ": freeVars = "
      <> T.pack (show (Set.toList (freeVars lambda)))
      <> ", params = "
      <> T.pack (show (params lambda))
      <> ", captured = "
      <> T.pack (show (Set.toList captured))
      <> ", union = "
      <> T.pack (show (Set.toList unionFree))
      <> ", filtered = "
      <> T.pack (show filtered)
  return $
    DStruct
      { dName = "env_" <> T.pack (show (lambdaId lambda)),
        dMembers = filtered
      }

constructEnvCode :: LiftedLambda -> T.Text -> CStmt
constructEnvCode lambda dest =
  SExpr $
    EMacroCall
      "OBJECT_ENV_OBJ_NEW"
      [ EVar dest,
        EVar $ "struct env_" <> T.pack (show $ lambdaId lambda)
      ]

merge xs ys = concatMap (\(x, y) -> [x, y]) (zip xs ys)

makeEnvCode :: LiftedLambda -> CExpr -> Codegen CExpr
makeEnvCode lambda parentEnv = do
  envVar <- genVar
  addStmt $ constructEnvCode lambda envVar
  let envAccess = generateEnvCast lambda (EVar envVar)
      parentEnvAccess = parentEnv
      captured = Set.fromList (filter (not . T.isPrefixOf "_unused") (params lambda))
      toCopy = Set.union (freeVars lambda) captured
      freeVarsList = Set.toList toCopy
  traverse_
    ( \var ->
        addStmt $
          SExpr $
            EBinOp
              "="
              (EArrow envAccess (nameForFreeVar var))
              (EArrow parentEnvAccess (nameForFreeVar var))
    )
    freeVarsList
  return $ EVar envVar

generateClosure :: LiftedLambda -> CExpr -> Codegen CExpr
generateClosure lambda currentEnv = do
  logDebugN $ "Generating closure for lambda: " <> T.pack (show $ lambdaId lambda)
  (closureExpr, newStmts) <- withStmtScope $ do
    envExpr <- makeEnvCode lambda currentEnv

    let initName = case length (params lambda) of
          1 -> "OBJECT_CLOSURE_ONE_NEW"
          2 -> "OBJECT_CLOSURE_TWO_NEW"
          n -> error $ "closure had " ++ show n ++ " parameters"

    varName <- genVar
    addStmt $
      SExpr $
        EMacroCall
          initName
          [ EVar varName,
            EVar $ "lambda_" <> T.pack (show $ lambdaId lambda),
            envExpr
          ]
    return $ EVar varName

  traverse_ addStmt newStmts
  return closureExpr

generateEnvCast :: LiftedLambda -> CExpr -> CExpr
generateEnvCast lambda expr =
  ECast
    (EPreUnOp "&" (EArrow expr "env"))
    (TPtr $ TStruct $ "env_" <> T.pack (show $ lambdaId lambda))

filterEnvAssignments :: (MonadLogger m) => [CStmt] -> Set.Set T.Text -> m [CStmt]
filterEnvAssignments stmts allowedSet =
  mapM filterStmt stmts
  where
    filterStmt stmt = case stmt of
      SExpr (EBinOp "=" lhs rhs) ->
        case lhs of
          EArrow _ attr | not (attr `Set.member` allowedSet) -> do
            logDebugN $ "=== filterEnvAssignments stmt: " <> T.pack (show stmt) <> " ==="
            logDebugN $ "=== filterEnvAssignments allowedSet: " <> T.pack (show allowedSet) <> " ==="
            return $ SExpr (ELitInt 0)
          _ -> return stmt
      _ -> return stmt

mapExceptLast :: (a -> a) -> [a] -> [a]
mapExceptLast _ [] = []
mapExceptLast _ [x] = [x]
mapExceptLast f (x : xs) = f x : mapExceptLast f xs

mapExceptLastM :: (Monad m) => (T.Text -> m (T.Text, T.Text)) -> [T.Text] -> m [(T.Text, T.Text)]
mapExceptLastM _ [] = return []
mapExceptLastM _ [_] = return [] -- Single element case returns empty list since we don't transform the last
mapExceptLastM f (x : xs@(_ : _)) = do
  y <- f x
  ys <- mapExceptLastM f xs
  return (y : ys)

-- transform :: T.Text -> IO (T.Text, T.Text)
-- transform t = return (t, T.append t "_suffix")

generateFunc :: LiftedLambda -> Codegen ()
generateFunc lambda = do
  logDebugN $ "=== generateFunc for lambda: " <> T.pack (show (lambdaId lambda)) <> " ==="
  -- Compute the "captured" parameters (those not marked unused)
  paramNames <-
    mapM
      ( \p -> do
          pn <- genVar
          return ((nameForFreeVar p), pn)
      )
      (params lambda)

  paramNamesToTypes <-
    mapM
      ( \(_, n) -> do
          return (n, objectType)
      )
      paramNames

  let captured = Set.fromList (fmap (\(o, n) -> o) (filter (\(o, n) -> (not (T.isPrefixOf "v__unused" o))) paramNames))
      freeVarNames = Set.fromList (fmap nameForFreeVar (Set.toList (freeVars lambda)))
      allowedSet = Set.union (Set.union freeVarNames captured) (Set.fromList ["env"])
      envObjType = TPtr (TStruct "env_obj")
      allParams = paramNamesToTypes ++ [("env_in", envObjType)]
  logDebugN $
    "generateFunc for lambda "
      <> T.pack (show (lambdaId lambda))
      <> ": captured = "
      <> T.pack (show (Set.toList captured))
      <> ", allowedSet = "
      <> T.pack (show (Set.toList allowedSet))
      <> ", params = "
      <> T.pack (show (params lambda))
      <> ", paramNames = "
      <> T.pack (show paramNames)
      <> ", paramNamesToTypes = "
      <> T.pack (show paramNamesToTypes)
      <> ", allParams= "
      <> T.pack (show allParams)
  addProto
    DFunProto
      { dName = "lambda_" <> T.pack (show (lambdaId lambda)),
        dRetType = TVoid,
        dParams = allParams,
        dNoreturn = True
      }
  envVarName <- genVar -- Generate a fresh variable name before entering the scope
  (bodyExpr, bodyStmts) <- withStmtScope $ do
    let envMove =
          SDecl $
            DVar
              { dName = envVarName,
                dType = TPtr (TStruct ("env_" <> T.pack (show (lambdaId lambda)))),
                dInit = Just $ generateEnvCast lambda (EVar "env_in")
              }
    addStmt envMove

    -- Add parameter cell initialization
    forM_ paramNames $ \(dv, iv) -> do
      -- Skip unused parameters
      when (dv `Set.member` allowedSet) $ do
        tmpVar <- genVar

        -- Create new cell object
        addStmt $
          SExpr $
            EMacroCall
              "OBJECT_CELL_OBJ_NEW"
              [ EVar tmpVar,
                EVar iv
              ]

        -- Assign to environment
        addStmt $
          SExpr $
            EBinOp
              "="
              (EArrow (EVar envVarName) dv)
              (EVar tmpVar)

    -- Generate the lambda body using the new environment variable.
    expr <- doCodegen (body lambda) (EVar envVarName)
    addStmt $ SExpr expr
    addStmt $ SExpr $ EMacroCall "__builtin_unreachable" []
    return expr
  -- Filter out any assignments in the generated body that copy fields not in allowedSet.
  filteredStmts <- filterEnvAssignments bodyStmts allowedSet
  -- Record the environment variable for this lambda.
  lift $ modify $ \ctx ->
    ctx {ctxLambdaEnvs = HashMap.insert (lambdaId lambda) envVarName (ctxLambdaEnvs ctx)}
  addDecl
    DFun
      { dName = "lambda_" <> T.pack (show (lambdaId lambda)),
        dRetType = TVoid,
        dParams = allParams,
        dBody = filteredStmts
      }

builtinIdentCodegen :: T.Text -> Codegen CExpr
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
        "not" -> (2, "not_k")
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

  (varName, stmts) <- withStmtScope $ do
    name <- genVar
    addStmt $
      SExpr $
        EMacroCall
          initName
          [ EVar name,
            EVar runtimeName,
            ENull
          ]
    return name

  traverse_ addStmt stmts
  return $ EVar varName

doCodegen :: LExpr -> CExpr -> Codegen CExpr
doCodegen expr currentEnv = do
  logDebugN $ "doCodegen processing expression: " <> T.pack (show expr)
  case expr of
    Var name -> do
      logDebugN $ "Processing variable: " <> name
      return $
        EArrow
          ( ECast
              (EArrow currentEnv (nameForFreeVar name))
              (TPtr $ TStruct "cell_obj")
          )
          "val"
    Lit lit -> do
      logDebugN $ "Processing literal: " <> T.pack (show lit)
      case lit of
        LString s -> do
          dest <- genVar
          addStmt $
            SExpr $
              EMacroCall
                "OBJECT_STRING_OBJ_NEW"
                [EVar dest, ELitStr s]
          return $ EVar dest
        LInt i -> do
          dest <- genVar
          addStmt $
            SExpr $
              EMacroCall
                "OBJECT_INT_OBJ_NEW"
                [EVar dest, ELitInt i]
          return $ EVar dest
        LNil -> return ENull
    BuiltinIdent ident -> builtinIdentCodegen ident
    SetThen var val cont -> do
      -- First do environment setup for the value if it's a closure
      valExpr <- doCodegen val currentEnv
      let resolvedName = nameForFreeVar var
      -- Get the cell ready
      cellVar <- genVar
      addStmt $
        SExpr $
          EMacroCall
            "OBJECT_CELL_OBJ_NEW"
            [ EVar cellVar,
              valExpr
            ]
      -- Do the set using the cell we created
      let setStmt =
            SExpr $
              EBinOp
                "="
                (EArrow currentEnv resolvedName)
                (EVar cellVar)
      addStmt setStmt
      -- Process continuation
      doCodegen cont currentEnv
    Lifted id -> do
      logDebugN $ "Processing Lifted: " <> T.pack (show id)
      ctx <- get
      case HashMap.lookup id (ctxLambdas ctx) of
        Just lambda -> generateClosure lambda currentEnv
        Nothing -> error $ "Unknown lambda id: " ++ show id
    If cond thenExpr elseExpr -> do
      condExpr <- doCodegen cond currentEnv
      thenFinal <- doCodegen thenExpr currentEnv
      elseFinal <- doCodegen elseExpr currentEnv

      addStmt $
        SIf
          (EMacroCall "obj_is_truthy" [condExpr])
          (SBlock [SExpr thenFinal])
          (SBlock [SExpr elseFinal])

      return $ ELitInt 0
    CallOne f arg -> do
      fExpr <- doCodegen f currentEnv
      argExpr <- doCodegen arg currentEnv
      return $ EMacroCall "call_closure_one" [fExpr, argExpr]
    CallTwo f arg1 arg2 -> do
      logDebugN $ "=== CallTwo:  " <> T.pack (show f) <> " ==="
      logDebugN $ "  CallTwo currentEnv:  " <> T.pack (show currentEnv)
      logDebugN $ "  CallTwo arg1:  " <> T.pack (show arg1)
      logDebugN $ "  CallTwo arg2:  " <> T.pack (show arg2)
      fExpr <- doCodegen f currentEnv
      logDebugN $ "  CallTwo fExpr:  " <> T.pack (show fExpr)
      arg1Expr <- doCodegen arg1 currentEnv
      logDebugN $ "  CallTwo arg1Expr:  " <> T.pack (show arg1Expr)
      arg2Expr <- doCodegen arg2 currentEnv
      logDebugN $ "  CallTwo arg2Expr:  " <> T.pack (show arg2Expr)
      return $ EMacroCall "call_closure_two" [fExpr, arg1Expr, arg2Expr]

codegen :: LExpr -> HashMap Int LiftedLambda -> IO ([CStmt], [CDecl], [CDecl])
codegen expr lambdas = do
  let ctx = initialCtx lambdas
  (finalExpr, finalCtx) <-
    runStateT
      ( runStderrLoggingT $ do
          logDebugN $ "Starting codegen with lambdas: " <> T.pack (show (HashMap.keys lambdas))
          traverse_
            ( \l -> do
                logDebugN $ "Processing lambda " <> T.pack (show (lambdaId l))
                -- Do not union in any extra free–variables from the parent.
                struct <- envStruct l
                addProto struct
                generateFunc l
            )
            (HashMap.elems lambdas)
          logDebugN "Starting main doCodegen"
          (expr', mainStmts) <- withStmtScope $ do
            result <- doCodegen expr (EVar "env")
            addStmt $ SExpr result
            return result
          logDebugN $ "Completed main doCodegen with " <> T.pack (show (length mainStmts)) <> " statements"
          traverse_ addStmt mainStmts
          return expr'
      )
      ctx
  return
    ( reverse $ ctxStmts finalCtx,
      reverse $ ctxProtos finalCtx,
      reverse $ ctxDecls finalCtx
    )
