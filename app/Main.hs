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
import System.Console.Haskeline
import System.Directory (copyFile, removeDirectoryRecursive, createDirectoryIfMissing)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
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
import Zayin.LiftedExpr.Pretty (renderLExpr, renderLiftedLambda)
import Zayin.Literals
import Zayin.Macros (expandMacros)
import Zayin.Parser (parseProgram)
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

invokeMake :: FilePath -> Bool -> IO (Either String String)
invokeMake tmpDir sanitize = do
  putStrLn $ "\n=== Executing make in: " ++ tmpDir ++ " ==="

  let args = if sanitize then ["SANITIZE=asan"] else []

  (_, mout, merr, ph) <- createProcess
    (proc "make" args) {
      cwd = Just tmpDir,
      std_out = CreatePipe,
      std_err = CreatePipe
    }

  -- Always log command execution
  putStrLn $ "Executing: make " ++ unwords args

  exitCode <- waitForProcess ph
  stdout <- maybe (return "") TIO.hGetContents mout
  stderr <- maybe (return "") TIO.hGetContents merr

  -- Always log all output
  putStrLn $ "\nMake stdout:\n" ++ T.unpack stdout
  putStrLn $ "\nMake stderr:\n" ++ T.unpack stderr
  putStrLn $ "\nMake exit code: " ++ show exitCode

  case exitCode of
    ExitSuccess -> return $ Right $ T.unpack stdout
    ExitFailure code ->
      return $ Left $ "Make failed with exit code: " ++ show code ++
                     "\nstdout: " ++ T.unpack stdout ++
                     "\nstderr: " ++ T.unpack stderr

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
          "    struct closure_obj initial_closure = object_closure_one_new(main_lambda, NULL);",
          "    struct thunk initial_thunk = {",
          "        .closr = &initial_closure,",
          "        .one = {NULL},",
          "    };",
          "",
          "    struct thunk *thnk_heap = mi_malloc(sizeof(struct thunk));",
          "    memcpy(thnk_heap, &initial_thunk, sizeof(struct thunk));",
          "    zayin_start(thnk_heap);",
          "}"
        ]
    ]

-- Make a REPL-specific wrapper to automatically display the last expression
wrapForRepl :: T.Text -> T.Text
wrapForRepl input =
  -- If the input is a simple expression (not a macro, function, etc.), add 'display' to it
  if T.all (\c -> c /= '\n') input && not (T.isPrefixOf "macro " $ T.strip input) && not (T.isPrefixOf "fn " $ T.strip input)
    then "display(" <> input <> ")"
    else input

--------------------------------------------------------------------------------
-- Compilation Function
--------------------------------------------------------------------------------

type CompilationM = LoggingT IO

runCompilation :: CompilationM a -> IO a
runCompilation = runStderrLoggingT

compileSource :: Options -> FilePath -> T.Text -> Bool -> IO (Either String FilePath)
compileSource opts buildDir src isRepl = runCompilation $ do
  -- Possibly transform the source for REPL
  let finalSrc = if isRepl then wrapForRepl src else src

  logInfoN $ "\nSource being compiled:\n" <> finalSrc

  -- Parse the source
  parsedEither <- liftIO $ parseProgram finalSrc
  parsedAST <- case parsedEither of
    Right ast -> do
      logInfoN $ "\nParsed input program into AST:\n" <> T.pack (show ast)
      logInfoN $ "\nPrettified AST: " <> T.pack (renderExpr ast)
      return ast
    Left err -> do
      logErrorN $ "Program parsing failed: " <> T.pack err
      liftIO exitFailure

  -- Macro expansion
  expandedExpr <- case expandMacros parsedAST of
    Right e -> do
      logInfoN $ "\nExpanded AST after macro processing:" <> T.pack (renderExpr e)
      return e
    Left err -> do
      logErrorN $ "Macro expansion failed: " <> T.pack err
      liftIO exitFailure

  -- Rest of the processing pipeline
  let initial = Gen id
      (boundExpr, state1) = runState (toBoundExprM expandedExpr) initial
  logInfoN $ "\nBound Expression: " <> T.pack (renderBExpr boundExpr)

  let k = CPS.BuiltinIdent "exit"
      (fExpr, _) = runState (toFExprM boundExpr k) state1
  logInfoN $ "\nAfter CPS conversion: " <> T.pack (renderFExpr fExpr)

  let (e, lambdas) = liftLambdas fExpr
  logInfoN $ "\nFinal expr before codegen:" <> T.pack (renderLExpr e)
  traverse_
    ( \(k', v) -> do
        logInfoN $ "Lambda " <> T.pack (show k') <> ":"
        logDebugN $ T.pack (renderLiftedLambda v)
    )
    (toList lambdas)

  (rootStmts, protos, decls) <- liftIO $ codegen e lambdas
  logInfoN $ "\nCodegen Context:"
  traverse_ (\s -> logInfoN $ "\nRoot statement: " <> T.pack (show s)) rootStmts
  traverse_ (\p -> logInfoN $ "\nProto: " <> T.pack (show p)) protos
  traverse_ (\d -> logInfoN $ "\nDecl: " <> T.pack (show d)) decls

  -- Generate C code
  let cCode = generateC rootStmts protos decls
      fullSource = generateProgramSource cCode

  logInfoN $ "\nGenerated C Code: " <> fullSource

  -- Write C code
  liftIO $ insertSourceIntoBuildDir buildDir fullSource
  makeResult <- liftIO $ invokeMake buildDir (sanitize opts)

  case makeResult of
    Left errMsg -> do
      logErrorN $ "\nFailed compiling generated C code: " <> T.pack errMsg
      return $ Left $ "Compilation failed: " ++ errMsg
    Right stdoutStr -> do
      logInfoN "\nCompilation result:"
      when (debug opts) $ logDebugN (T.pack stdoutStr)
      return $ Right $ buildDir </> "compiled_result"

--------------------------------------------------------------------------------
-- REPL Implementation
--------------------------------------------------------------------------------

data ReplState = ReplState
  { buildDir :: FilePath
  , history :: [T.Text]  -- Previous inputs
  , environment :: [T.Text]  -- Definitions (macros, functions) to persist
  }

runRepl :: Options -> IO ()
runRepl opts = do
  -- Create persistent build directory for the REPL
  dir <- createReplBuildDir

  -- Set up initial state
  let initialState = ReplState
        { buildDir = dir
        , history = []
        , environment = []
        }

  putStrLn "Zayin REPL v0.1"
  putStrLn "Type expressions to evaluate, :help for commands"

  -- Run REPL loop using Haskeline for history and editing
  finally (runInputT defaultSettings (replLoop opts initialState)) $ do
    -- Cleanup on exit
    unless (keepTmpdir opts) $ removeDirectoryRecursive dir
    putStrLn "Goodbye!"

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

replLoop :: Options -> ReplState -> InputT IO ()
replLoop opts state = do
  minput <- getInputLine "zayin> "
  case minput of
    Nothing -> return ()  -- EOF
    Just ":q" -> return ()  -- Quit
    Just ":quit" -> return ()  -- Quit
    Just ":exit" -> return ()  -- Quit
    Just ":help" -> do
      outputStrLn "Commands:"
      outputStrLn "  :q, :quit, :exit - Exit the REPL"
      outputStrLn "  :help           - Show this help message"
      outputStrLn "  :reset          - Reset environment (clear all definitions)"
      outputStrLn "  :history        - Show input history"
      outputStrLn "  :env            - Show current environment"
      replLoop opts state
    Just ":reset" -> do
      outputStrLn "Environment reset."
      replLoop opts state { environment = [] }
    Just ":history" -> do
      outputStrLn "Input history:"
      forM_ (zip [1..] (reverse $ history state)) $ \(i, input) ->
        outputStrLn $ show (i :: Int) ++ ": " ++ T.unpack input
      replLoop opts state
    Just ":env" -> do
      outputStrLn "Current environment:"
      forM_ (environment state) $ \def ->
        outputStrLn $ T.unpack def
      replLoop opts state
    Just input
      | T.null (T.strip (T.pack input)) -> replLoop opts state
      | otherwise -> do
          let inputText = T.pack input
          -- Combine environment with new input
          let fullProgram = T.unlines (environment state) <> inputText

          -- Run compiler and execution
          result <- liftIO $ compileSource opts (buildDir state) fullProgram True
          case result of
            Left err -> do
              outputStrLn $ "Error: " ++ err
              -- Continue the REPL loop with updated history
              replLoop opts $ state { history = inputText : history state }

            Right exePath -> do
              -- Run the compiled program
              exitCode <- liftIO $ runProgramWithErrorHandling exePath
              when (exitCode /= ExitSuccess) $
                liftIO $ hPutStrLn stderr "Execution failed!"

              -- Continue the REPL loop with updated state
              let newState = updateState inputText state
              replLoop opts $ newState { history = inputText : history newState }

--------------------------------------------------------------------------------
-- Main Function
--------------------------------------------------------------------------------

main :: IO ()
main = do
  opts <- execParser optsInfo

  case cmd opts of
    Repl -> runRepl opts

    Run -> case sourceFile opts of
      Nothing -> putStrLn "Error: Source file required for 'run' command" >> exitFailure
      Just srcFile -> do
        src <- TIO.readFile srcFile
        buildDir <- generateBuildDir

        result <- compileSource opts buildDir src False
        case result of
          Left err -> putStrLn ("Compilation failed: " ++ err) >> exitFailure
          Right exePath -> do
            -- Run the compiled program
            callProcess exePath []

            -- Cleanup
            unless (keepTmpdir opts) $ removeDirectoryRecursive buildDir

    Compile {output = out} -> case sourceFile opts of
      Nothing -> putStrLn "Error: Source file required for 'compile' command" >> exitFailure
      Just srcFile -> do
        src <- TIO.readFile srcFile
        buildDir <- generateBuildDir

        result <- compileSource opts buildDir src False
        case result of
          Left err -> putStrLn ("Compilation failed: " ++ err) >> exitFailure
          Right exePath -> do
            -- Copy the binary to the output location
            copyBinary buildDir out
            putStrLn $ "Compiled binary written to " ++ out

            -- Cleanup
            unless (keepTmpdir opts) $ removeDirectoryRecursive buildDir
