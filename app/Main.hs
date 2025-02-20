{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception (catch, SomeException)
import Control.Monad (when)
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Foldable (forM_, traverse_)
import Data.HashMap.Strict (toList)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Directory (copyFile, removeDirectoryRecursive)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import System.Process (ProcessHandle, StdStream (..), callProcess, createProcess, cwd, proc, std_err, std_out, waitForProcess)
import Text.Parsec (parse)

-- Import your own modules (make sure these are available in your project)
import Zayin.AST
import Zayin.AST.Pretty
import Zayin.BoundExpr.Pretty
import Zayin.CExport (generateC)
import qualified Zayin.CPS as CPS
import Zayin.Codegen (codegen)
import Zayin.FlatExpr (liftLambdas)
import Zayin.Literals
import Zayin.Transforms (toFExprM)
import Zayin.Parser (parseProgram)  -- your top-level parser

--------------------------------------------------------------------------------
-- Command-line Options
--------------------------------------------------------------------------------

-- Your original command type remains (Run vs Compile with an output file)
data Cmd = Run | Compile { output :: FilePath }
  deriving (Show)

-- Extend Options to include a required positional argument for the source file.
data Options = Options
  { cmd        :: Cmd
  , sourceFile :: FilePath   -- new: path to the .zai source file
  , debug      :: Bool
  , keepTmpdir :: Bool
  }
  deriving (Show)

cmdParser :: Parser Cmd
cmdParser = subparser
  ( command "run" (info (pure Run)
       ( progDesc "Run the program" ))
 <> command "compile" (info (Compile <$> strOption
       ( long "output"
      <> short 'o'
      <> metavar "OUTPUT"
      <> value "a.out"
      <> help "Output binary file" ))
       ( progDesc "Compile the program" ))
  )

optionsParser :: Parser Options
optionsParser = Options
  <$> cmdParser
  <*> argument str
        ( metavar "SOURCE"
       <> help "Input source file (.zai)" )
  <*> switch ( long "debug" <> help "Enable debug output" )
  <*> switch ( long "keep-tmpdir" <> help "Keep temporary build directory" )

optsInfo :: ParserInfo Options
optsInfo = info (optionsParser <**> helper)
  ( fullDesc
 <> progDesc "Compile or run the Zayin program"
 <> header "zayin - A Zayin compiler" )


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
    ("queue_thunk.h", $(makeRelativeToProject "runtime/queue_thunk.h" >>= embedFile)),
    ("scheduler.h", $(makeRelativeToProject "runtime/scheduler.h" >>= embedFile)),
    ("scheduler.c", $(makeRelativeToProject "runtime/scheduler.c" >>= embedFile)),
    ("thread_context.h", $(makeRelativeToProject "runtime/thread_context.h" >>= embedFile)),
    ("thread_context.c", $(makeRelativeToProject "runtime/thread_context.c" >>= embedFile)),
    ("test_queue.c", $(makeRelativeToProject "runtime/test_queue.c" >>= embedFile)),
    ("vec.h", $(makeRelativeToProject "runtime/vec.h" >>= embedFile)),
    ("Makefile", $(makeRelativeToProject "runtime/Makefile" >>= embedFile))
  ]

copyBinary :: FilePath -> FilePath -> IO ()
copyBinary tmpDir outputPath =
  copyFile (tmpDir </> "compiled_result") outputPath `catch` handler
  where
    handler :: SomeException -> IO ()
    handler _ = error "failed copying compiled binary"

invokeMake :: FilePath -> IO (Either String String)
invokeMake tmpDir = do
  putStrLn $ "\nExecuting make in: " ++ tmpDir
  (_, mout, merr, ph) <-
    createProcess
      (proc "make" ["SANITIZE=asan"])
        { cwd = Just tmpDir,
          std_out = CreatePipe,
          std_err = CreatePipe
        }
  exitCode <- waitForProcess ph
  stdout <- maybe (return "") TIO.hGetContents mout
  stderr <- maybe (return "") TIO.hGetContents merr
  case exitCode of
    ExitSuccess -> return $ Right $ T.unpack stdout
    ExitFailure code ->
      return $
        Left $
          "Make failed with exit code: "
            ++ show code
            ++ "\nstdout: "
            ++ T.unpack stdout
            ++ "\nstderr: "
            ++ T.unpack stderr

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
        [ "#include <stdlib.h>",
          "#include <string.h>",
          "#include<unistd.h>",
          "#include \"base.h\"",
          "#include \"builtin.h\"",
          "#include \"scheduler.h\""
        ],
      src,
      T.unlines
        [ "int main(void) {"
        , "    start_scheduler();  // Start the scheduler (worker threads run in background)"
        , "    // Schedule the initial thunk (e.g., the computation for 1+1)"
        , "    struct closure_obj initial_closure = object_closure_one_new(main_lambda, NULL);"
        , "    struct thunk initial_thunk = {"
        , "        .closr = &initial_closure,"
        , "        .one = {NULL},"
        , "    };"
        , ""
        , "    struct thunk *thnk_heap = malloc(sizeof(struct thunk));"
        , "    memcpy(thnk_heap, &initial_thunk, sizeof(struct thunk));"
        , "    schedule_thunk(thnk_heap);"
        , ""
        , "    // Main thread can now either wait, process further work, or exit."
        , "    while (1) {"
        , "        sleep(1);"
        , "    }"
        , "    return 0;"
        , "}"
        ]
        -- [ "int main() {",
        --   "  start_scheduler();",
        --   "  struct closure_obj initial_closure = object_closure_one_new(main_lambda, NULL);",
        --   "  struct thunk initial_thunk = {",
        --   "    .closr = &initial_closure,",
        --   "    .one = {NULL},",
        --   "  };",
        --   "",
        --   "  struct thunk *thnk_heap = malloc(sizeof(struct thunk));",
        --   "  memcpy(thnk_heap, &initial_thunk, sizeof(struct thunk));",
        --   "  schedule_thunk(thnk_heap);",
        --   "  return 0;",
        --   "}"
        -- ]
    ]

--------------------------------------------------------------------------------
-- Main Compilation/Execution Pipeline
--------------------------------------------------------------------------------

main :: IO ()
main = do
  opts <- execParser optsInfo
  putStrLn "Starting compilation process..."

  -- Read the source file provided on the command line
  src <- TIO.readFile (sourceFile opts)

  putStrLn("\nSource being compiled:\n" ++ T.unpack src)

  -- Parse the file using your top-level parser.
  -- case parse parseProgram "" src of
  --   Left err -> do
  --     putStrLn ("Parse error:\n" ++ show err)
  --     exitFailure
  --   Right expr -> do
  --     putStrLn "High-level AST:"
  --     print expr

  let
    wrappedExpr =
      EApp
        (ELam [] (ExprBody
          { bodyExprs = [ Def "addOne" (ELam ["x"] (ExprBody [] (EApp (EVar "+") [EVar "x", ELit (LInt 1)]))) ]
          , finalExpr = EApp (EVar "addOne") [ELit (LInt 1)]
          }))
        []

      -- EApp (ELam [] (ExprBody [] expr)) []  -- wrapping the AST
  putStrLn "Wrapped AST:"
  putStrLn (renderExpr wrappedExpr)

  -- Original processing of the AST:
  let initial = Gen id
      (boundExpr, state1) = runState (toBoundExprM wrappedExpr) initial
  putStrLn "\nBound Expression:"
  putStrLn (renderBExpr boundExpr)

  let
      k = CPS.BuiltinIdent "exit"
      (fExpr, _) = runState (toFExprM boundExpr k) state1
  putStrLn "\nAfter Conversion:"
  print fExpr

  let (e, lambdas) = liftLambdas fExpr
  putStrLn "\nFinal expr before codegen:"
  traverse_ (\(k', v) -> putStrLn $ "Lambda " ++ show k' ++ ":\n" ++ show v) (toList lambdas)

  let (rootStmts, protos, decls) = codegen e lambdas
  putStrLn "\nCodegen Context:"
  traverse_ (\s -> putStrLn $ "Root statement: " ++ show s) rootStmts
  traverse_ (\p -> putStrLn $ "Proto: " ++ show p) protos
  traverse_ (\d -> putStrLn $ "Decl: " ++ show d) decls

  -- Generate the build directory and write the generated C code:
  buildDir <- generateBuildDir
  let cCode = generateC rootStmts protos decls
      fullSource = generateProgramSource cCode
  putStrLn "\nGenerated C Code:"
  TIO.putStrLn fullSource
  insertSourceIntoBuildDir buildDir fullSource

  -- Invoke make to build the C code:
  makeResult <- invokeMake buildDir
  case makeResult of
    Left errMsg -> do
      putStrLn $ "\nFailed compiling generated C code: " ++ errMsg
      exitFailure
    Right stdoutStr -> do
      putStrLn "\nCompilation result:"
      putStrLn stdoutStr
      when (debug opts) $ putStrLn stdoutStr

  -- Depending on the command, either copy the binary or run it:
  case cmd opts of
    Compile { output = out } -> do
      copyBinary buildDir out `catch` handlerCopy
      putStrLn $ "Compiled binary copied to " ++ out
    Run -> do
      let exePath = buildDir </> "compiled_result"
      putStrLn $ "Running " ++ exePath
      callProcess exePath []

  -- Clean up the temporary build directory unless requested otherwise:
  if not (keepTmpdir opts)
    then do
      removeDirectoryRecursive buildDir
      putStrLn "Temporary build directory removed."
    else putStrLn $ "Temporary build directory kept: " ++ buildDir

  where
    handlerCopy :: SomeException -> IO ()
    handlerCopy _ = error "failed copying compiled binary"
