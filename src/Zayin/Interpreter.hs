{-# LANGUAGE OverloadedStrings #-}

module Zayin.Interpreter
  ( interpret
  , interpretWithEnv
  , Value(..)
  , Environment
  , emptyEnv
  , InterpreterError(..)
  , valueToString
  , LambdaMap       -- Export this type
  , emptyLambdaMap  -- Export an empty lambda map
  ) where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HashMap
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Set as Set
import System.Exit (exitWith, exitSuccess, ExitCode(..))

import Zayin.AST
import Zayin.Literals
import Zayin.LiftedExpr

-- Type alias for lambda maps
type LambdaMap = HashMap.HashMap Int LiftedLambda

-- Empty lambda map
emptyLambdaMap :: LambdaMap
emptyLambdaMap = HashMap.empty

-- Runtime value representation
data Value
  = VInt Integer
  | VString T.Text
  | VBool Bool
  | VNil
  | VClosure Environment Int               -- CPS-style closure
  | VBuiltinFunc T.Text ([Value] -> Interpreter Value)  -- Direct-style builtin
  | VCons Value Value                      -- For list operations
  | VNoop

instance Show Value where
  show (VInt i) = "VInt " ++ show i
  show (VString s) = "VString " ++ show s
  show (VBool b) = "VBool " ++ show b
  show VNil = "VNil"
  show VNoop = "VNoop"
  show (VClosure _ id) = "VClosure <env> " ++ show id
  show (VBuiltinFunc name _) = "VBuiltinFunc " ++ T.unpack name  -- Don't try to show the function
  show (VCons car cdr) = "VCons (" ++ show car ++ ") (" ++ show cdr ++ ")"

-- Error types
data InterpreterError
  = UnboundVariable T.Text
  | TypeMismatch String Value
  | UnknownLambda Int
  | UnknownBuiltin T.Text
  | ApplicationError String
  deriving (Show)

-- Environment maps variable names to values
type Environment = Map.Map T.Text Value

-- Interpreter monad with access to all lambda definitions
type Interpreter a = ExceptT InterpreterError (ReaderT LambdaMap (StateT InterpreterState IO)) a

-- Interpreter state
data InterpreterState = InterpreterState
  { globalEnv :: Environment
  , currentEnv :: Environment
  , replMode :: Bool
  , lastValue :: Value
  , debugMode :: Bool  -- Add debugging flag
  }

-- Initial empty environment
emptyEnv :: Environment
emptyEnv = Map.empty

-- Initialize interpreter state
initialState :: Bool -> Bool -> InterpreterState
initialState isRepl isDebug = InterpreterState
  { globalEnv = builtinEnv
  , currentEnv = Map.empty
  , replMode = isRepl
  , lastValue = VNil
  , debugMode = isDebug
  }

-- Helper function to find mangled variables
findMangledVariable :: T.Text -> Environment -> Maybe Value
findMangledVariable name env =
  -- First try exact match
  case Map.lookup name env of
    Just val -> Just val
    Nothing ->
      -- Then try with any numeric suffix
      let prefix = name `T.append` "$"
          candidates = filter (\k -> prefix `T.isPrefixOf` k) (Map.keys env)
      in case candidates of
          [] -> Nothing
          (k:_) -> Map.lookup k env

-- Logging function for debugging
logDebug :: String -> Interpreter ()
logDebug msg = do
  st <- get
  when (debugMode st) $ liftIO $ putStrLn $ "DEBUG: " ++ msg

-- Environment logging helper
logEnv :: String -> Environment -> Interpreter ()
logEnv label env = do
  st <- get
  when (debugMode st) $ do
    liftIO $ putStrLn $ "DEBUG: " ++ label ++ " keys: " ++ show (Map.keys env)
    liftIO $ putStrLn $ "DEBUG: " ++ label ++ " values: " ++ show (map showValueType (Map.elems env))
  where
    showValueType :: Value -> String
    showValueType (VInt _) = "Int"
    showValueType (VString _) = "String"
    showValueType (VBool _) = "Bool"
    showValueType VNil = "Nil"
    showValueType (VClosure _ id) = "Closure " ++ show id
    showValueType (VBuiltinFunc name _) = "Builtin " ++ T.unpack name
    showValueType (VCons _ _) = "Cons"
    showValueType VNoop = "Noop"

-- Builtin function registry
builtinEnv :: Environment
builtinEnv = Map.fromList
  [ ("+", makeBuiltin "+" addFunc)
  , ("-", makeBuiltin "-" subFunc)
  , ("*", makeBuiltin "*" mulFunc)
  , ("/", makeBuiltin "/" divFunc)
  , ("%", makeBuiltin "%" modFunc)
  , ("<", makeBuiltin "<" ltFunc)
  , ("<=", makeBuiltin "<=" leqFunc)
  , (">", makeBuiltin ">" gtFunc)
  , (">=", makeBuiltin ">=" geqFunc)
  , ("not", makeBuiltin "not" notFunc)
  , ("eq?", makeBuiltin "eq?" eqFunc)
  , ("cons", makeBuiltin "cons" consFunc)
  , ("car", makeBuiltin "car" carFunc)
  , ("cdr", makeBuiltin "cdr" cdrFunc)
  , ("cons?", makeBuiltin "cons?" isConsFunc)
  , ("null?", makeBuiltin "null?" isNullFunc)
  , ("display", makeBuiltin "display" displayFunc)
  , ("tostring", makeBuiltin "tostring" toStringFunc)
  , ("string-concat", makeBuiltin "string-concat" stringConcatFunc)
  , ("exit", makeBuiltin "exit" exitFunc)
  -- Add more builtins as needed
  ]
  where
    makeBuiltin name func = VBuiltinFunc name func

-- Execute a lifted expression
evalExpr :: LExpr -> Environment -> Interpreter Value
evalExpr expr env = case expr of
  Var name -> do
    logDebug $ "Looking up variable: " ++ T.unpack name
    logEnv "Local environment" env
    st <- get
    logEnv "Global environment" (globalEnv st)

    -- First check local environment with mangling-aware lookup
    case findMangledVariable name env of
      Just val -> do
        logDebug $ "Found " ++ T.unpack name ++ " in local environment: " ++ show val
        return val
      Nothing -> do
        -- Then check global environment with mangling-aware lookup
        state <- get
        case findMangledVariable name (globalEnv state) of
          Just val -> do
            logDebug $ "Found " ++ T.unpack name ++ " in global environment: " ++ show val
            return val
          Nothing -> do
            logDebug $ "Variable not found: " ++ T.unpack name
            throwError $ UnboundVariable name

  Lit lit -> do
    let val = case lit of
          LInt n -> VInt n
          LString s -> VString s
          LBool b -> VBool b
          LNil -> VNil
    logDebug $ "Evaluating literal: " ++ show val
    return val

  BuiltinIdent name -> do
    logDebug $ "Looking up builtin: " ++ T.unpack name
    case Map.lookup name builtinEnv of
      Just val -> do
        logDebug $ "Found builtin: " ++ T.unpack name
        return val
      Nothing -> do
        logDebug $ "Unknown builtin: " ++ T.unpack name
        throwError $ UnknownBuiltin name

  SetThen var valExpr contExpr -> do
    logDebug $ "Setting variable: " ++ T.unpack var
    val <- evalExpr valExpr env
    logDebug $ "Value of " ++ T.unpack var ++ ": " ++ show val

    let env' = Map.insert var val env
    logDebug $ "Updated local environment with: " ++ T.unpack var

    -- Also update the global environment
    oldState <- get
    logEnv "Global environment before update" (globalEnv oldState)

    modify $ \s -> s { globalEnv = Map.insert var val (globalEnv s) }

    newState <- get
    logEnv "Global environment after update" (globalEnv newState)

    evalExpr contExpr env'

  If condExpr thenExpr elseExpr -> do
    logDebug "Evaluating if condition"
    condVal <- evalExpr condExpr env
    logDebug $ "Condition result: " ++ show condVal
    case condVal of
      VBool False -> do
        logDebug "Taking else branch"
        evalExpr elseExpr env
      VNil -> do
        logDebug "Taking else branch (condition was nil)"
        evalExpr elseExpr env
      _ -> do
        logDebug "Taking then branch"
        evalExpr thenExpr env

  Lifted lambdaId -> do
    logDebug $ "Creating closure with lambda id: " ++ show lambdaId
    -- Create a closure with current environment and lambda id
    return $ VClosure env lambdaId

  -- In CPS, CallOne means f(arg) where f already has its continuation
  CallOne funcExpr argExpr -> do
    logDebug "Evaluating function in CallOne"
    funcVal <- evalExpr funcExpr env
    logDebug $ "Function value: " ++ show funcVal
    logDebug "Evaluating argument in CallOne"
    argVal <- evalExpr argExpr env
    logDebug $ "Argument value: " ++ show argVal

    case funcVal of
      -- For builtins, we need special handling as they're not in CPS form
      VBuiltinFunc name impl -> do
        logDebug $ "Calling builtin: " ++ T.unpack name
        -- For a unary function with implicit continuation (like exit)
        impl [argVal]

      -- For user-defined closures in CPS form
      VClosure closureEnv lambdaId -> do
        logDebug $ "Calling closure with lambda id: " ++ show lambdaId
        lambdas <- ask
        case HashMap.lookup lambdaId lambdas of
          Nothing -> do
            logDebug $ "Unknown lambda: " ++ show lambdaId
            throwError $ UnknownLambda lambdaId
          Just lambda -> do
            -- In CPS, the function already knows its continuation
            -- So we just call it with its argument
            logDebug "Setting up call environment"
            let callEnv = setupCallEnv lambda closureEnv [argVal]
            logDebug "Evaluating lambda body"
            evalExpr (body lambda) callEnv

      other -> do
        logDebug $ "Cannot apply: " ++ valueToString other
        throwError $ ApplicationError $ "Cannot apply: " ++ valueToString other

  -- In CPS, CallTwo means f(arg, cont) - second arg is the continuation
  CallTwo funcExpr arg1Expr contExpr -> do
    logDebug "Evaluating function in CallTwo"
    funcVal <- evalExpr funcExpr env
    logDebug $ "Function value: " ++ show funcVal
    logDebug "Evaluating arg1 in CallTwo"
    arg1Val <- evalExpr arg1Expr env
    logDebug $ "Arg1 value: " ++ show arg1Val
    logDebug "Evaluating continuation in CallTwo"
    contVal <- evalExpr contExpr env
    logDebug $ "Continuation value: " ++ show contVal

    case funcVal of
      -- For builtins that take a continuation
      VBuiltinFunc name impl -> do
        logDebug $ "Calling builtin with continuation: " ++ T.unpack name
        -- Execute the builtin
        result <- impl [arg1Val]
        -- Then invoke the continuation with the result
        logDebug "Applying continuation to builtin result"
        applyContinuation contVal result

      -- For user-defined closures
      VClosure closureEnv lambdaId -> do
        logDebug $ "Calling closure with lambda id: " ++ show lambdaId
        lambdas <- ask
        case HashMap.lookup lambdaId lambdas of
          Nothing -> do
            logDebug $ "Unknown lambda: " ++ show lambdaId
            throwError $ UnknownLambda lambdaId
          Just lambda -> do
            -- The continuation is the second parameter in CPS form
            logDebug "Setting up call environment with continuation"
            let callEnv = setupCallEnv lambda closureEnv [arg1Val, contVal]
            logDebug "Evaluating lambda body"
            evalExpr (body lambda) callEnv

      other -> do
        logDebug $ "Cannot apply: " ++ valueToString other
        throwError $ ApplicationError $ "Cannot apply: " ++ valueToString other

-- Helper to set up the environment for a function call
setupCallEnv :: LiftedLambda -> Environment -> [Value] -> Environment
setupCallEnv lambda closureEnv args =
  let
    -- Bind parameters to arguments
    paramBindings = zip (params lambda) args
    -- Start with an empty environment
    emptyEnv = Map.empty
    -- Add parameter bindings
    paramEnv = foldr (\(p, a) e -> Map.insert p a e) emptyEnv paramBindings
    -- Add free variables from closure
    freeVarsEnv = foldr
                   (\fv e ->
                     case Map.lookup fv closureEnv of
                       Just val -> Map.insert fv val e
                       Nothing -> e)
                   paramEnv
                   (Set.toList $ freeVars lambda)
  in
    freeVarsEnv

-- Helper to apply a continuation to a value
applyContinuation :: Value -> Value -> Interpreter Value
applyContinuation contVal argVal =
  case contVal of
    VClosure closureEnv lambdaId -> do
      logDebug $ "Applying continuation with lambda id: " ++ show lambdaId
      lambdas <- ask
      case HashMap.lookup lambdaId lambdas of
        Nothing -> do
          logDebug $ "Unknown lambda for continuation: " ++ show lambdaId
          throwError $ UnknownLambda lambdaId
        Just lambda -> do
          logDebug "Setting up continuation environment"
          let callEnv = setupCallEnv lambda closureEnv [argVal]
          logDebug "Evaluating continuation body"
          evalExpr (body lambda) callEnv

    VBuiltinFunc name impl -> do
      logDebug $ "Applying builtin continuation: " ++ T.unpack name
      impl [argVal]

    other -> do
      logDebug $ "Cannot use as continuation: " ++ valueToString other
      throwError $ ApplicationError $ "Cannot use as continuation: " ++ valueToString other


-- Builtin function implementations

-- Numeric operations with CPS awareness
-- Arithmetic operations with CPS awareness
addFunc :: [Value] -> Interpreter Value
addFunc [VInt x] =
  -- Handle single argument case - return a function waiting for second arg
  return $ VBuiltinFunc "+_partial" $ \args -> case args of
    [VInt y] -> return $ VInt (x + y)
    [y, cont@(VClosure _ _)] -> do
      case y of
        VInt n -> do
          let result = VInt (x + n)
          applyContinuation cont result
        _ -> throwError $ TypeMismatch "Expected integer as second argument to +" y
    _ -> throwError $ ApplicationError $ "Invalid arguments to partially applied +: " ++ show args
addFunc [VInt x, VInt y] = return $ VInt (x + y)
addFunc [VInt x, contVal@(VClosure _ _)] =
  return $ VBuiltinFunc "+" $ \[arg] -> case arg of
    VInt y -> do
      let result = VInt (x + y)
      applyContinuation contVal result
    _ -> throwError $ TypeMismatch "Expected integer as second argument to +" arg
addFunc args@[val, cont@(VClosure _ _)] =
  throwError $ TypeMismatch "Expected integer as first argument to +" val
addFunc args =
  throwError $ ApplicationError $ "Invalid arguments to +: " ++ show args

subFunc :: [Value] -> Interpreter Value
subFunc [VInt x] =
  return $ VBuiltinFunc "-_partial" $ \args -> case args of
    [VInt y] -> return $ VInt (x - y)
    [y, cont@(VClosure _ _)] -> do
      case y of
        VInt n -> do
          let result = VInt (x - n)
          applyContinuation cont result
        _ -> throwError $ TypeMismatch "Expected integer as second argument to -" y
    _ -> throwError $ ApplicationError $ "Invalid arguments to partially applied -: " ++ show args
subFunc [VInt x, VInt y] = return $ VInt (x - y)
subFunc [VInt x, contVal@(VClosure _ _)] =
  return $ VBuiltinFunc "-" $ \[arg] -> case arg of
    VInt y -> do
      let result = VInt (x - y)
      applyContinuation contVal result
    _ -> throwError $ TypeMismatch "Expected integer as second argument to -" arg
subFunc args@[val, cont@(VClosure _ _)] =
  throwError $ TypeMismatch "Expected integer as first argument to -" val
subFunc args =
  throwError $ ApplicationError $ "Invalid arguments to -: " ++ show args

mulFunc :: [Value] -> Interpreter Value
mulFunc [VInt x] =
  return $ VBuiltinFunc "*_partial" $ \args -> case args of
    [VInt y] -> return $ VInt (x * y)
    [y, cont@(VClosure _ _)] -> do
      case y of
        VInt n -> do
          let result = VInt (x * n)
          applyContinuation cont result
        _ -> throwError $ TypeMismatch "Expected integer as second argument to *" y
    _ -> throwError $ ApplicationError $ "Invalid arguments to partially applied *: " ++ show args
mulFunc [VInt x, VInt y] = return $ VInt (x * y)
mulFunc [VInt x, contVal@(VClosure _ _)] =
  return $ VBuiltinFunc "*" $ \[arg] -> case arg of
    VInt y -> do
      let result = VInt (x * y)
      applyContinuation contVal result
    _ -> throwError $ TypeMismatch "Expected integer as second argument to *" arg
mulFunc args@[val, cont@(VClosure _ _)] =
  throwError $ TypeMismatch "Expected integer as first argument to *" val
mulFunc args =
  throwError $ ApplicationError $ "Invalid arguments to *: " ++ show args

divFunc :: [Value] -> Interpreter Value
divFunc [VInt x] =
  return $ VBuiltinFunc "/_partial" $ \args -> case args of
    [VInt 0] -> throwError $ ApplicationError "Division by zero"
    [VInt y] -> return $ VInt (x `div` y)
    [y, cont@(VClosure _ _)] -> do
      case y of
        VInt 0 -> throwError $ ApplicationError "Division by zero"
        VInt n -> do
          let result = VInt (x `div` n)
          applyContinuation cont result
        _ -> throwError $ TypeMismatch "Expected integer as second argument to /" y
    _ -> throwError $ ApplicationError $ "Invalid arguments to partially applied /: " ++ show args
divFunc [VInt x, VInt y] | y == 0 = throwError $ ApplicationError "Division by zero"
                         | otherwise = return $ VInt (x `div` y)
divFunc [VInt x, contVal@(VClosure _ _)] =
  return $ VBuiltinFunc "/" $ \[arg] -> case arg of
    VInt 0 -> throwError $ ApplicationError "Division by zero"
    VInt y -> do
      let result = VInt (x `div` y)
      applyContinuation contVal result
    _ -> throwError $ TypeMismatch "Expected integer as second argument to /" arg
divFunc args@[val, cont@(VClosure _ _)] =
  throwError $ TypeMismatch "Expected integer as first argument to /" val
divFunc args =
  throwError $ ApplicationError $ "Invalid arguments to /: " ++ show args

modFunc :: [Value] -> Interpreter Value
modFunc [VInt x] =
  return $ VBuiltinFunc "%_partial" $ \args -> case args of
    [VInt 0] -> throwError $ ApplicationError "Modulo by zero"
    [VInt y] -> return $ VInt (x `mod` y)
    [y, cont@(VClosure _ _)] -> do
      case y of
        VInt 0 -> throwError $ ApplicationError "Modulo by zero"
        VInt n -> do
          let result = VInt (x `mod` n)
          applyContinuation cont result
        _ -> throwError $ TypeMismatch "Expected integer as second argument to %" y
    _ -> throwError $ ApplicationError $ "Invalid arguments to partially applied %: " ++ show args
modFunc [VInt x, VInt y] | y == 0 = throwError $ ApplicationError "Modulo by zero"
                         | otherwise = return $ VInt (x `mod` y)
modFunc [VInt x, contVal@(VClosure _ _)] =
  return $ VBuiltinFunc "%" $ \[arg] -> case arg of
    VInt 0 -> throwError $ ApplicationError "Modulo by zero"
    VInt y -> do
      let result = VInt (x `mod` y)
      applyContinuation contVal result
    _ -> throwError $ TypeMismatch "Expected integer as second argument to %" arg
modFunc args@[val, cont@(VClosure _ _)] =
  throwError $ TypeMismatch "Expected integer as first argument to %" val
modFunc args =
  throwError $ ApplicationError $ "Invalid arguments to %: " ++ show args

-- Comparison operations with CPS awareness
ltFunc :: [Value] -> Interpreter Value
ltFunc [VInt x] =
  return $ VBuiltinFunc "<_partial" $ \args -> case args of
    [VInt y] -> return $ VBool (x < y)
    [y, cont@(VClosure _ _)] -> do
      case y of
        VInt n -> do
          let result = VBool (x < n)
          applyContinuation cont result
        _ -> throwError $ TypeMismatch "Expected integer as second argument to <" y
    _ -> throwError $ ApplicationError $ "Invalid arguments to partially applied <: " ++ show args
ltFunc [VInt x, VInt y] = return $ VBool (x < y)
ltFunc [VInt x, contVal@(VClosure _ _)] =
  return $ VBuiltinFunc "<" $ \[arg] -> case arg of
    VInt y -> do
      let result = VBool (x < y)
      applyContinuation contVal result
    _ -> throwError $ TypeMismatch "Expected integer as second argument to <" arg
ltFunc args@[val, cont@(VClosure _ _)] =
  throwError $ TypeMismatch "Expected integer as first argument to <" val
ltFunc args =
  throwError $ ApplicationError $ "Invalid arguments to <: " ++ show args

leqFunc :: [Value] -> Interpreter Value
leqFunc [VInt x] =
  return $ VBuiltinFunc "<=_partial" $ \args -> case args of
    [VInt y] -> return $ VBool (x <= y)
    [y, cont@(VClosure _ _)] -> do
      case y of
        VInt n -> do
          let result = VBool (x <= n)
          applyContinuation cont result
        _ -> throwError $ TypeMismatch "Expected integer as second argument to <=" y
    _ -> throwError $ ApplicationError $ "Invalid arguments to partially applied <=: " ++ show args
leqFunc [VInt x, VInt y] = return $ VBool (x <= y)
leqFunc [VInt x, contVal@(VClosure _ _)] =
  return $ VBuiltinFunc "<=" $ \[arg] -> case arg of
    VInt y -> do
      let result = VBool (x <= y)
      applyContinuation contVal result
    _ -> throwError $ TypeMismatch "Expected integer as second argument to <=" arg
leqFunc args@[val, cont@(VClosure _ _)] =
  throwError $ TypeMismatch "Expected integer as first argument to <=" val
leqFunc args =
  throwError $ ApplicationError $ "Invalid arguments to <=: " ++ show args

gtFunc :: [Value] -> Interpreter Value
gtFunc [VInt x] =
  return $ VBuiltinFunc ">_partial" $ \args -> case args of
    [VInt y] -> return $ VBool (x > y)
    [y, cont@(VClosure _ _)] -> do
      case y of
        VInt n -> do
          let result = VBool (x > n)
          applyContinuation cont result
        _ -> throwError $ TypeMismatch "Expected integer as second argument to >" y
    _ -> throwError $ ApplicationError $ "Invalid arguments to partially applied >: " ++ show args
gtFunc [VInt x, VInt y] = return $ VBool (x > y)
gtFunc [VInt x, contVal@(VClosure _ _)] =
  return $ VBuiltinFunc ">" $ \[arg] -> case arg of
    VInt y -> do
      let result = VBool (x > y)
      applyContinuation contVal result
    _ -> throwError $ TypeMismatch "Expected integer as second argument to >" arg
gtFunc args@[val, cont@(VClosure _ _)] =
  throwError $ TypeMismatch "Expected integer as first argument to >" val
gtFunc args =
  throwError $ ApplicationError $ "Invalid arguments to >: " ++ show args

geqFunc :: [Value] -> Interpreter Value
geqFunc [VInt x] =
  return $ VBuiltinFunc ">=_partial" $ \args -> case args of
    [VInt y] -> return $ VBool (x >= y)
    [y, cont@(VClosure _ _)] -> do
      case y of
        VInt n -> do
          let result = VBool (x >= n)
          applyContinuation cont result
        _ -> throwError $ TypeMismatch "Expected integer as second argument to >=" y
    _ -> throwError $ ApplicationError $ "Invalid arguments to partially applied >=: " ++ show args
geqFunc [VInt x, VInt y] = return $ VBool (x >= y)
geqFunc [VInt x, contVal@(VClosure _ _)] =
  return $ VBuiltinFunc ">=" $ \[arg] -> case arg of
    VInt y -> do
      let result = VBool (x >= y)
      applyContinuation contVal result
    _ -> throwError $ TypeMismatch "Expected integer as second argument to >=" arg
geqFunc args@[val, cont@(VClosure _ _)] =
  throwError $ TypeMismatch "Expected integer as first argument to >=" val
geqFunc args =
  throwError $ ApplicationError $ "Invalid arguments to >=: " ++ show args

-- Logic operations with CPS awareness
notFunc :: [Value] -> Interpreter Value
notFunc [VBool b] = return $ VBool (not b)
notFunc [VNil] = return $ VBool True
notFunc [_] = return $ VBool False
notFunc [val, contVal@(VClosure _ _)] = do
  result <- notFunc [val]
  applyContinuation contVal result
notFunc args =
  throwError $ ApplicationError $ "Invalid arguments to not: " ++ show args

eqFunc :: [Value] -> Interpreter Value
eqFunc [x] =
  return $ VBuiltinFunc "eq?_partial" $ \args -> case args of
    [y] -> return $ VBool (valueEquals x y)
    [y, cont@(VClosure _ _)] -> do
      let result = VBool (valueEquals x y)
      applyContinuation cont result
    _ -> throwError $ ApplicationError $ "Invalid arguments to partially applied eq?: " ++ show args
eqFunc [x, y] = return $ VBool (valueEquals x y)
eqFunc [x, contVal@(VClosure _ _)] =
  return $ VBuiltinFunc "eq?" $ \[arg] -> do
    let result = VBool (valueEquals x arg)
    applyContinuation contVal result
eqFunc args =
  throwError $ ApplicationError $ "Invalid arguments to eq?: " ++ show args

-- List operations with CPS awareness
consFunc :: [Value] -> Interpreter Value
consFunc [car] =
  return $ VBuiltinFunc "cons_partial" $ \args -> case args of
    [cdr] -> return $ VCons car cdr
    [cdr, cont@(VClosure _ _)] -> do
      let result = VCons car cdr
      applyContinuation cont result
    _ -> throwError $ ApplicationError $ "Invalid arguments to partially applied cons: " ++ show args
consFunc [car, cdr] = return $ VCons car cdr
consFunc [car, contVal@(VClosure _ _)] =
  return $ VBuiltinFunc "cons" $ \[arg] -> do
    let result = VCons car arg
    applyContinuation contVal result
consFunc args =
  throwError $ ApplicationError $ "Invalid arguments to cons: " ++ show args

carFunc :: [Value] -> Interpreter Value
carFunc [VCons car _] = return car
carFunc [VCons car _, contVal@(VClosure _ _)] =
  applyContinuation contVal car
carFunc [other] =
  throwError $ TypeMismatch "Expected cons cell for car" other
carFunc [other, _] =
  throwError $ TypeMismatch "Expected cons cell for car" other
carFunc args =
  throwError $ ApplicationError $ "Invalid arguments to car: " ++ show args

cdrFunc :: [Value] -> Interpreter Value
cdrFunc [VCons _ cdr] = return cdr
cdrFunc [VCons _ cdr, contVal@(VClosure _ _)] =
  applyContinuation contVal cdr
cdrFunc [other] =
  throwError $ TypeMismatch "Expected cons cell for cdr" other
cdrFunc [other, _] =
  throwError $ TypeMismatch "Expected cons cell for cdr" other
cdrFunc args =
  throwError $ ApplicationError $ "Invalid arguments to cdr: " ++ show args

isConsFunc :: [Value] -> Interpreter Value
isConsFunc [VCons _ _] = return $ VBool True
isConsFunc [_] = return $ VBool False
isConsFunc [val, contVal@(VClosure _ _)] = do
  result <- isConsFunc [val]
  applyContinuation contVal result
isConsFunc args =
  throwError $ ApplicationError $ "Invalid arguments to cons?: " ++ show args

isNullFunc :: [Value] -> Interpreter Value
isNullFunc [VNil] = return $ VBool True
isNullFunc [_] = return $ VBool False
isNullFunc [val, contVal@(VClosure _ _)] = do
  result <- isNullFunc [val]
  applyContinuation contVal result
isNullFunc args =
  throwError $ ApplicationError $ "Invalid arguments to null?: " ++ show args

-- I/O operations with CPS awareness
displayFunc :: [Value] -> Interpreter Value
displayFunc [val] = do
  liftIO $ putStrLn $ valueToString val
  state <- get
  modify $ \s -> s { lastValue = VNoop }  -- Use VNoop to indicate nothing should be printed
  return VNoop  -- Return VNoop to indicate no printing needed
displayFunc [val, contVal@(VClosure _ _)] = do
  liftIO $ putStrLn $ valueToString val
  state <- get
  modify $ \s -> s { lastValue = VNoop }
  applyContinuation contVal VNoop  -- Pass VNoop to the continuation
displayFunc args =
  throwError $ ApplicationError $ "Invalid arguments to display: " ++ show args

toStringFunc :: [Value] -> Interpreter Value
toStringFunc [val] = return $ VString $ T.pack $ valueToString val
toStringFunc [val, contVal@(VClosure _ _)] = do
  result <- toStringFunc [val]
  applyContinuation contVal result
toStringFunc args =
  throwError $ ApplicationError $ "Invalid arguments to tostring: " ++ show args

-- String operations with CPS awareness
stringConcatFunc :: [Value] -> Interpreter Value
stringConcatFunc [VString s1] =
  return $ VBuiltinFunc "string-concat_partial" $ \args -> case args of
    [VString s2] -> return $ VString (s1 <> s2)
    [s2, cont@(VClosure _ _)] -> case s2 of
      VString s2val -> do
        let result = VString (s1 <> s2val)
        applyContinuation cont result
      _ -> throwError $ TypeMismatch "Expected string as second argument" s2
    _ -> throwError $ ApplicationError $ "Invalid arguments to partially applied string-concat: " ++ show args
stringConcatFunc [VString s1, VString s2] =
  return $ VString (s1 <> s2)
stringConcatFunc [VString s1, contVal@(VClosure _ _)] =
  return $ VBuiltinFunc "string-concat" $ \[arg] -> case arg of
    VString s2 -> do
      let result = VString (s1 <> s2)
      applyContinuation contVal result
    _ -> throwError $ TypeMismatch "Expected string as second argument to string-concat" arg
stringConcatFunc args@[val, cont@(VClosure _ _)] =
  throwError $ TypeMismatch "Expected string as first argument to string-concat" val
stringConcatFunc args =
  throwError $ ApplicationError $ "Invalid arguments to string-concat: " ++ show args

-- System operations (terminal operations - may not invoke continuation)
exitFunc :: [Value] -> Interpreter Value
exitFunc [val] = do
  state <- get
  if replMode state
    then do
      -- Store the value in the state and return it
      modify $ \s -> s { lastValue = val }
      return val
    else liftIO $ System.Exit.exitWith $ System.Exit.ExitFailure (fromIntegral $
      case val of
        VInt code -> code
        _ -> 0)
exitFunc [] = do
  state <- get
  if replMode state
    then do
      -- Store nil in the state for empty exit call
      modify $ \s -> s { lastValue = VNil }
      return VNil
    else liftIO $ System.Exit.exitSuccess
exitFunc [VInt code, contVal@(VClosure _ _)] = do
  state <- get
  if replMode state
    then do
      -- Store the integer value before continuing
      modify $ \s -> s { lastValue = VInt code }
      applyContinuation contVal VNil
    else liftIO $ System.Exit.exitWith $ System.Exit.ExitFailure (fromIntegral code)
exitFunc [val, contVal@(VClosure _ _)] = do
  state <- get
  if replMode state
    then do
      -- Store the value before continuing
      modify $ \s -> s { lastValue = val }
      applyContinuation contVal VNil
    else liftIO $ System.Exit.exitSuccess
exitFunc args =
  throwError $ ApplicationError $ "Invalid arguments to exit: " ++ show args

-- Value equality check
valueEquals :: Value -> Value -> Bool
valueEquals (VInt a) (VInt b) = a == b
valueEquals (VString a) (VString b) = a == b
valueEquals (VBool a) (VBool b) = a == b
valueEquals VNil VNil = True
valueEquals (VCons a1 b1) (VCons a2 b2) = valueEquals a1 a2 && valueEquals b1 b2
valueEquals _ _ = False

-- Enhanced valueToString to support cons cells
valueToString :: Value -> String
valueToString (VInt i) = show i
valueToString (VString s) = T.unpack s
valueToString (VBool True) = "#t"
valueToString (VBool False) = "#f"
valueToString VNil = "nil"
valueToString (VClosure _ _) = "<function>"
valueToString (VBuiltinFunc name _) = "<builtin:" ++ T.unpack name ++ ">"
valueToString (VCons car cdr) = "(" ++ valueToString car ++ " . " ++ valueToString cdr ++ ")"
valueToString VNoop = "<no output>"

-- Main interpretation function
interpret :: LExpr -> LambdaMap -> Bool -> IO (Either InterpreterError Value)
interpret expr lambdaMap isRepl = do
  liftIO $ putStrLn ("DEBUG: Starting interpret with replMode = " ++ show isRepl)

  (result, finalState) <- runStateT
                            (runReaderT
                              (runExceptT (evalExpr expr Map.empty))
                              lambdaMap)
                            (initialState isRepl False)

  liftIO $ putStrLn ("DEBUG: interpret finished with result: " ++ either show show result)
  liftIO $ putStrLn ("DEBUG: Final global env keys: " ++ show (Map.keys (globalEnv finalState)))

  -- Return either the error or the stored last value
  return $ case result of
    Left err -> Left err
    Right _ -> Right (lastValue finalState)  -- Return last value instead of result

-- Updated to also return the lambda map
interpretWithEnv :: LExpr -> LambdaMap -> Bool -> Environment -> IO (Either String (Value, Environment, LambdaMap))
interpretWithEnv expr lambdaMap isRepl initialEnv = do
  liftIO $ putStrLn ("DEBUG: Starting interpretWithEnv with replMode = " ++ show isRepl)
  liftIO $ putStrLn ("DEBUG: Initial env keys: " ++ show (Map.keys initialEnv))
  liftIO $ putStrLn ("DEBUG: Initial lambda map size: " ++ show (HashMap.size lambdaMap))

  let initState = InterpreterState
        { globalEnv = Map.union initialEnv builtinEnv  -- Use provided environment and merge with builtins
        , currentEnv = Map.empty
        , replMode = isRepl
        , lastValue = VNil
        , debugMode = True  -- Enable debug mode
        }

  liftIO $ putStrLn ("DEBUG: Initial global env keys after merging with builtins: " ++ show (Map.keys (globalEnv initState)))

  (result, finalState) <- runStateT
                            (runReaderT
                              (runExceptT (evalExpr expr Map.empty))
                              lambdaMap)
                            initState

  let resultMsg = case result of
        Left err -> "Error: " ++ show err
        Right _ -> "Success: " ++ valueToString (lastValue finalState)

  liftIO $ putStrLn ("DEBUG: interpretWithEnv finished with result: " ++ resultMsg)
  liftIO $ putStrLn ("DEBUG: Final global env keys: " ++ show (Map.keys (globalEnv finalState)))

  -- Return the result value, updated environment, AND the lambda map
  return $ case result of
    Left err -> Left (show err)  -- Convert InterpreterError to String
    Right _ -> Right (lastValue finalState, globalEnv finalState, lambdaMap)
