{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception (SomeException, catch, finally)
import Control.Monad (unless, when, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logDebugN, logErrorN, logInfoN, runStderrLoggingT)
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Foldable (forM_, traverse_)
import Data.HashMap.Strict (toList)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Console.ANSI
import System.Console.Haskeline
import System.Directory (copyFile, removeDirectoryRecursive, createDirectoryIfMissing)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr, stdout)
import System.IO.Temp (createTempDirectory)
import System.Process (ProcessHandle, StdStream (..), callProcess, createProcess, cwd, proc, std_err, std_out, waitForProcess)

-- Import your own modules
import Zayin.AST
import Zayin.AST.Pretty
import Zayin.BoundExpr.Pretty
import Zayin.CExport (generateC)
import qualified Zayin.CPS as CPS
import Zayin.Codegen (codegen)
import Zayin.FlatExpr (liftLambdas)
import Zayin.FlatExpr.Pretty (renderFExpr)
import Zayin.Interpreter (emptyEnv, interpretWithEnv, valueToString, Environment(..), Value(..))
import Zayin.LiftedExpr.Pretty (renderLExpr, renderLiftedLambda)
import Zayin.Literals
import Zayin.Macros (expandMacros)
import Zayin.Parser (parseProgram, parseWithDebug)
import Zayin.Transforms (toFExprM)

--------------------------------------------------------------------------------
-- Command-line Options
--------------------------------------------------------------------------------

data Cmd = Run | Compile {output :: FilePath} | Repl
  deriving (Show)

data Options = Options
  { cmd :: Cmd,
    sourceFile :: Maybe FilePath, -- optional for REPL mode
    debug :: Bool,
    keepTmpdir :: Bool,
    sanitize :: Bool
  }
  deriving (Show)

cmdParser :: Parser Cmd
cmdParser =
  subparser
    ( command
        "run"
        ( info
            (pure Run)
            (progDesc "Run the program")
        )
        <> command
          "compile"
          ( info
              ( Compile
                  <$> strOption
                    ( long "output"
                        <> short 'o'
                        <> metavar "OUTPUT"
                        <> value "a.out"
                        <> help "Output binary file"
                    )
              )
              (progDesc "Compile the program")
          )
        <> command
          "repl"
          ( info
              (pure Repl)
              (progDesc "Start interactive REPL")
          )
    )

optionsParser :: Parser Options
optionsParser =
  Options
    <$> cmdParser
    <*> optional (argument
                   str
                   (metavar "SOURCE" <> help "Input source file (.zyn)"))
    <*> switch (long "debug" <> help "Enable debug output")
    <*> switch (long "keep-tmpdir" <> help "Keep temporary build directory")
    <*> switch (long "sanitize" <> help "Build with ASAN sanitizer")

optsInfo :: ParserInfo Options
optsInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Compile, run, or start REPL for Zayin programs"
        <> header "zayin - A Zayin compiler and REPL"
    )

-- | Runtime files embedded at compile time
runtimeFiles :: [(FilePath, BS.ByteString)]
runtimeFiles =
  [ ("base.h", $(makeRelativeToProject "runtime/base.h" >>= embedFile)),
    ("base.c", $(makeRelativeToProject "runtime/base.c" >>= embedFile)),
    ("builtin.h", $(makeRelativeToProject "runtime/builtin.h" >>= embedFile)),
    ("builtin.c", $(makeRelativeToProject "runtime/builtin.c" >>= embedFile)),
    ("bit_array.h", $(makeRelativeToProject "runtime/bit_array.h" >>= embedFile)),
    ("bit_array.c", $(makeRelativeToProject "runtime/bit_array.c" >>= embedFile)),
    ("common.h", $(makeRelativeToProject "runtime/common.h" >>= embedFile)),
    ("gc.h", $(makeRelativeToProject "runtime/gc.h" >>= embedFile)),
    ("gc.c", $(makeRelativeToProject "runtime/gc.c" >>= embedFile)),
    ("hash_table.h", $(makeRelativeToProject "runtime/hash_table.h" >>= embedFile)),
    ("queue.h", $(makeRelativeToProject "runtime/queue.h" >>= embedFile)),
    ("test_queue.c", $(makeRelativeToProject "runtime/test_queue.c" >>= embedFile)),
    ("vec.h", $(makeRelativeToProject "runtime/vec.h" >>= embedFile)),
    ("Makefile", $(makeRelativeToProject "runtime/Makefile" >>= embedFile))
  ]

--------------------------------------------------------------------------------
-- Compiler Pipeline
--------------------------------------------------------------------------------

-- Create a persistent build directory for the REPL
createReplBuildDir :: IO FilePath
createReplBuildDir = do
  -- Create a build directory in the current directory
  let buildDir = ".zayin_repl"
  createDirectoryIfMissing True buildDir

  -- Copy runtime files
  forM_ runtimeFiles $ \(path, contents) -> do
    let fullPath = buildDir </> path
    BS.writeFile fullPath contents

  return buildDir

copyBinary :: FilePath -> FilePath -> IO ()
copyBinary tmpDir outputPath =
  copyFile (tmpDir </> "compiled_result") outputPath `catch` handler
  where
    handler :: SomeException -> IO ()
    handler e = error "failed copying compiled binary"

invokeMake :: FilePath -> Bool -> Bool -> IO (Either String String)
invokeMake tmpDir sanitize debugMode = do
  when debugMode $ putStrLn $ "\n=== Executing make in: " ++ tmpDir ++ " ==="

  let args = if sanitize then ["SANITIZE=asan"] else []

  (_, mout, merr, ph) <- createProcess
    (proc "make" args) {
      cwd = Just tmpDir,
      std_out = CreatePipe,
      std_err = CreatePipe
    }

  -- Log command execution only in debug mode
  when debugMode $ putStrLn $ "Executing: make " ++ unwords args

  exitCode <- waitForProcess ph
  stdout <- maybe (return "") TIO.hGetContents mout
  stderr <- maybe (return "") TIO.hGetContents merr

  -- Log outputs only in debug mode
  when debugMode $ do
    putStrLn $ "\nMake stdout:\n" ++ T.unpack stdout
    putStrLn $ "\nMake stderr:\n" ++ T.unpack stderr
    putStrLn $ "\nMake exit code: " ++ show exitCode

  case exitCode of
    ExitSuccess ->
      if debugMode
        then return $ Right $ T.unpack stdout
        else return $ Right ""  -- Empty string when not in debug mode, since we don't need the output
    ExitFailure code ->
      return $ Left $ "Make failed with exit code: " ++ show code ++
                     (if debugMode
                       then "\nstdout: " ++ T.unpack stdout ++ "\nstderr: " ++ T.unpack stderr
                       else "")  -- Simplified error message when not in debug mode

insertSourceIntoBuildDir :: FilePath -> T.Text -> IO ()
insertSourceIntoBuildDir tmpDir source = do
  let tmpPath = tmpDir </> "compiled_result.c"
  TIO.writeFile tmpPath source

generateBuildDir :: IO FilePath
generateBuildDir = do
  tmpDir <- createTempDirectory "." "zayin"
  forM_ runtimeFiles $ \(path, contents) -> do
    let fullPath = tmpDir </> path
    BS.writeFile fullPath contents
  return tmpDir

generateProgramSource :: T.Text -> T.Text
generateProgramSource src =
  T.concat
    [ T.unlines
        [ "#include <mimalloc.h>",
          "#include <stdlib.h>",
          "#include <string.h>",
          "#include <unistd.h>",
          "#include \"base.h\"",
          "#include \"builtin.h\""
        ],
      src,
      T.unlines
        [ "int main(void) {",
          "    mi_version();",
          "    gc_thread_data *thd;",
          "    long stack_size = global_stack_size = STACK_SIZE;",
          "    long heap_size = global_heap_size = HEAP_SIZE;",
          "    mclosure0(clos_exit,&zyn_exit);",
          "    mclosure0(entry_pt,&c_entry_pt);",
          "    gc_initialize();",
          "    thd = mi_malloc(sizeof(gc_thread_data));",
          "    gc_thread_data_init(thd, 0, (char *) &stack_size, stack_size);",
          "    thd->gc_cont = &entry_pt;",
          "    thd->gc_args[0] = &clos_halt;",
          "    thd->gc_num_args = 1;",
          "    thd->thread_id = pthread_self();",
          "    gc_add_mutator(thd);",
          "    Cyc_heap_init(heap_size);",
          "    thd->thread_state = CYC_THREAD_STATE_RUNNABLE;"
          "    zyn_start_trampoline(thd);",
          "    return 0;"
          "}"
        ]
    ]

--------------------------------------------------------------------------------
-- Compilation Function
--------------------------------------------------------------------------------

type CompilationM = LoggingT IO

runCompilation :: CompilationM a -> IO a
runCompilation = runStderrLoggingT

compileSource :: Options -> FilePath -> T.Text -> Bool -> IO (Either String FilePath)
compileSource opts buildDir src isRepl = runCompilation $ do
   -- Possibly transform the source for REPL
  let debugMode = debug opts

  when debugMode $ logInfoN $ "\nSource being compiled:\n" <> src

  -- Parse the source with debug flag
  parsedEither <- liftIO $ parseWithDebug debugMode src
  parsedAST <- case parsedEither of
    Right ast -> do
      when debugMode $ do
        logInfoN $ "\nParsed input program into AST:\n" <> T.pack (show ast)
        logInfoN $ "\nPrettified AST: " <> T.pack (renderExpr ast)
      return ast
    Left err -> do
      logErrorN $ "Program parsing failed: " <> T.pack err
      liftIO exitFailure

  -- Macro expansion
  expandedExpr <- case expandMacros parsedAST of
    Right e -> do
      when debugMode $
        logInfoN $ "\nExpanded AST after macro processing:" <> T.pack (renderExpr e)
      return e
    Left err -> do
      logErrorN $ "Macro expansion failed: " <> T.pack err
      liftIO exitFailure

  -- Rest of the processing pipeline - Updated to use runFresh and pass the debugMode flag
  let initial = Gen id

  -- Use runFresh instead of runState for toBoundExprM with debug flag
  (boundExpr, state1) <- liftIO $ runFresh (toBoundExprM debugMode expandedExpr) initial
  when debugMode $
    logInfoN $ "\nBound Expression: " <> T.pack (renderBExpr boundExpr)

  let k = CPS.BuiltinIdent "exit"

  -- Updated to use runFresh for toFExprM with debug flag
  (fExpr, _) <- liftIO $ runFresh (toFExprM debugMode boundExpr k) state1

  when debugMode $
    logInfoN $ "\nAfter CPS conversion: " <> T.pack (renderFExpr fExpr)

  -- Updated to pass debug flag to liftLambdas
  let (e, lambdas) = liftLambdas debugMode fExpr
  when debugMode $ do
    logInfoN $ "\nFinal expr before codegen:" <> T.pack (renderLExpr e)
    traverse_
      ( \(k', v) -> do
          logInfoN $ "Lambda " <> T.pack (show k') <> ":"
          logDebugN $ T.pack (renderLiftedLambda v)
      )
      (toList lambdas)

  -- Updated to pass debug flag to codegen
  (rootStmts, protos, decls) <- liftIO $ codegen debugMode e lambdas
  when debugMode $ do
    logInfoN $ "\nCodegen Context:"
    traverse_ (\s -> logInfoN $ "\nRoot statement: " <> T.pack (show s)) rootStmts
    traverse_ (\p -> logInfoN $ "\nProto: " <> T.pack (show p)) protos
    traverse_ (\d -> logInfoN $ "\nDecl: " <> T.pack (show d)) decls

  -- Generate C code
  let cCode = generateC rootStmts protos decls
      fullSource = generateProgramSource cCode

  when debugMode $
    logInfoN $ "\nGenerated C Code: " <> fullSource

  -- Write C code
  liftIO $ insertSourceIntoBuildDir buildDir fullSource
  makeResult <- liftIO $ invokeMake buildDir (sanitize opts) debugMode  -- Pass debug flag to invokeMake

  case makeResult of
    Left errMsg -> do
      logErrorN $ "\nFailed compiling generated C code: " <> T.pack errMsg
      return $ Left $ "Compilation failed: " ++ errMsg
    Right stdoutStr -> do
      when debugMode $ logInfoN "\nCompilation result:"
      when debugMode $ logDebugN (T.pack stdoutStr)
      return $ Right $ buildDir </> "compiled_result"

--------------------------------------------------------------------------------
-- REPL Implementation
--------------------------------------------------------------------------------

-- Display the banner, adapting to terminal capabilities
displayZayinBanner :: IO ()
displayZayinBanner = do
  -- Check if terminal supports colors and if colors are not disabled
  noColor <- (elem "NOCOLOR" . map fst) <$> getEnvironment
  supportsANSI <- hSupportsANSI stdout

  if supportsANSI && not noColor
    then displayColorBanner
    else displaySimpleBanner

-- Colored version
displayColorBanner :: IO ()
displayColorBanner = do
  setSGR [SetColor Foreground Vivid Blue]
  putStrLn zayinLogo
  setSGR [SetColor Foreground Vivid White]
  setSGR [Reset]
  putStrLn ""

-- Simple version without colors
displaySimpleBanner :: IO ()
displaySimpleBanner = do
  putStrLn zayinLogo
  putStrLn ""

-- The scaled-down logo
zayinLogo :: String
zayinLogo = unlines
  [ "                                                    "
  , "                                                    "
  , "                                                    "
  , "                     .-.                            "
  , "                   .-@@*.                           "
  , "                   .#@@@@@@@@@@#+.                  "
  , "                   :@@@@@@@@@@@@@@-.                "
  , "                   .#@@@@@@@@@@@@@+.                "
  , "                    .#@@@@@@@@@@@@+.                "
  , "                       ...*@-:...                   "
  , "                         -@%.                       "
  , "                        .%@-                        "
  , "                        :@@:                        "
  , "                       .#@@-                        "
  , "                       .@@@#.                       "
  , "                       .@@@@.                       "
  , "                       .@@@@=.                      "
  , "                       .%@@@*.                      "
  , "                        +@@@%:                      "
  , "                        .@@@@-                      "
  , "                        .@@@@-                      "
  , "                        .%@@@-                      "
  , "                        .%@@%:                      "
  , "                        .@@@:                       "
  , "                       .=+:                         "
  , "                                                    "
  , "                                                    "
  , "                                                    "
  ]

data ReplState = ReplState
  { buildDir :: FilePath
  , history :: [T.Text]  -- Previous inputs
  , environment :: [T.Text]  -- Definitions (macros, functions) to persist
  , interpreterEnv :: Environment
  , lambdaMap :: LambdaMap
  }

runRepl :: Options -> IO ()
runRepl opts = do
  -- Create persistent build directory for the REPL
  dir <- createReplBuildDir

  let debugMode = debug opts
  when debugMode $ putStrLn $ "REPL build directory: " ++ dir

  -- Set up initial state
  let initialState = ReplState
        { buildDir = dir
        , history = []
        , environment = []
        , interpreterEnv = emptyEnv
        , lambdaMap = emptyLambdaMap
        }

  displayZayinBanner
  putStrLn "Zayin REPL v0.1"
  putStrLn "Type expressions to evaluate, :help for commands"
  when debugMode $ putStrLn "[Debug mode enabled]"

  -- Run REPL loop using Haskeline for history and editing
  finally (runInputT defaultSettings (replLoop opts initialState)) $ do
    -- Cleanup on exit
    unless (keepTmpdir opts) $ do
      when debugMode $ putStrLn $ "Cleaning up REPL build directory: " ++ dir
      removeDirectoryRecursive dir
    putStrLn "Goodbye!"

replLoop :: Options -> ReplState -> InputT IO ()
replLoop opts state = do
  let debugMode = debug opts
  minput <- getInputLine "zayin> "
  case minput of
    -- Handle commands the same as before (no changes needed for :q, :help, etc.)

    -- Update the :reset command to clear the environment
    Just ":reset" -> do
      outputStrLn "Environment reset."
      replLoop opts state { environment = [], interpreterEnv = emptyEnv }

    -- Update the :env command to show environment content
    Just ":env" -> do
      outputStrLn "Current environment:"
      forM_ (environment state) $ \def ->
        outputStrLn $ T.unpack def
      replLoop opts state

    -- Handle normal expressions
    Just input
      | T.null (T.strip (T.pack input)) -> replLoop opts state
      | otherwise -> do
          let inputText = T.pack input

          -- Update history
          let stateWithHistory = state { history = inputText : history state }

          -- If it's a function/macro definition, also update the textual environment
          let updatedState = if isMacroOrFunctionDef inputText
                            then stateWithHistory { environment = environment stateWithHistory ++ [inputText] }
                            else stateWithHistory

          -- Run through the pipeline using existing approach
          result <- liftIO $ do
            parsedEither <- parseWithDebug debugMode inputText
            case parsedEither of
              Left err -> return $ Left $ "Parse error: " ++ err
              Right ast ->
                case expandMacros ast of
                  Left err -> return $ Left $ "Macro error: " ++ err
                  Right expandedAst -> do
                    -- Create a fresh generator
                    (boundExpr, state1) <- runFresh (toBoundExprM debugMode expandedAst) (Gen id)
                    let k = CPS.BuiltinIdent "exit"
                    -- Get flat expression from bound expression
                    (fExpr, _) <- runFresh (toFExprM debugMode boundExpr k) state1
                    -- Lift lambdas
                    let (lexpr, lambdas) = liftLambdas debugMode fExpr

                    -- Pass the current environment to interpret and get new environment back
                    interpretWithEnv lexpr lambdas True (interpreterEnv updatedState) (interpreterEnv updatedState)

        case result of
          Left err -> do
            outputStrLn err
            replLoop opts updatedState  -- Keep the same state on error
          Right (val, newEnv, newLambdaMap) -> do  -- Extract the lambda map too
            -- Store both the updated environment and lambda map
            let finalState = updatedState {
                  interpreterEnv = newEnv,
                  lambdaMap = newLambdaMap  -- Update lambda map
                }

            -- Only print the result if it's not VNoop
            case val of
              VNoop -> replLoop opts finalState
              _ -> do
                outputStrLn $ valueToString val
                replLoop opts finalState
          case result of
            Left err -> do
              outputStrLn err
              replLoop opts updatedState  -- Keep the same environment on error
            Right (val, newEnv) -> do
              -- Store the updated environment
              let finalState = updatedState { interpreterEnv = newEnv }

              -- Only print the result if it's not VNoop
              case val of
                VNoop -> replLoop opts finalState
                _ -> do
                  outputStrLn $ valueToString val
                  replLoop opts finalState

-- Helper function to determine if input contains a definition
updateState :: T.Text -> ReplState -> ReplState
updateState input state
  | isMacroOrFunctionDef input = state { environment = environment state ++ [input] }
  | otherwise = state

-- Check if the input is a macro or function definition that should be preserved
isMacroOrFunctionDef :: T.Text -> Bool
isMacroOrFunctionDef input =
  let stripped = T.stripStart input
  in T.isPrefixOf "macro " stripped || T.isPrefixOf "fn " stripped

-- Handle execution errors without using ScopedTypeVariables
runProgramWithErrorHandling :: FilePath -> IO ExitCode
runProgramWithErrorHandling exePath = do
  result <- catch
    (do
      callProcess exePath []
      return ExitSuccess)
    (\e -> do
       let _ = e :: SomeException  -- Type annotation in binding position is allowed
       return (ExitFailure 1))
  return result


--------------------------------------------------------------------------------
-- Main Function
--------------------------------------------------------------------------------

main :: IO ()
main = do
  opts <- execParser optsInfo

  -- Configure output based on debug flag
  let debugMode = debug opts
      -- Optional debug message for when running in debug mode
      debugMsg = when debugMode $
                   putStrLn "Debug mode enabled. Showing detailed compilation information."

  case cmd opts of
    Repl -> do
      debugMsg
      runRepl opts

    Run -> case sourceFile opts of
      Nothing -> putStrLn "Error: Source file required for 'run' command" >> exitFailure
      Just srcFile -> do
        debugMsg
        src <- TIO.readFile srcFile
        buildDir <- generateBuildDir

        when debugMode $ putStrLn $ "Created build directory: " ++ buildDir

        result <- compileSource opts buildDir src False
        case result of
          Left err -> putStrLn ("Compilation failed: " ++ err) >> exitFailure
          Right exePath -> do
            when debugMode $ putStrLn $ "Running compiled program: " ++ exePath
            -- Run the compiled program
            callProcess exePath []

            -- Cleanup
            unless (keepTmpdir opts) $ do
              when debugMode $ putStrLn $ "Cleaning up build directory: " ++ buildDir
              removeDirectoryRecursive buildDir

    Compile {output = out} -> case sourceFile opts of
      Nothing -> putStrLn "Error: Source file required for 'compile' command" >> exitFailure
      Just srcFile -> do
        debugMsg
        src <- TIO.readFile srcFile
        buildDir <- generateBuildDir

        when debugMode $ putStrLn $ "Created build directory: " ++ buildDir

        result <- compileSource opts buildDir src False
        case result of
          Left err -> putStrLn ("Compilation failed: " ++ err) >> exitFailure
          Right exePath -> do
            -- Copy the binary to the output location
            copyBinary buildDir out
            putStrLn $ "Compiled binary written to " ++ out

            -- Cleanup
            unless (keepTmpdir opts) $ do
              when debugMode $ putStrLn $ "Cleaning up build directory: " ++ buildDir
              removeDirectoryRecursive buildDir
