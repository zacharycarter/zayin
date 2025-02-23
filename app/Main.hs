{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception (SomeException, catch)
import Control.Monad (unless, when)
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
import Zayin.FlatExpr.Pretty (renderFExpr)
import Zayin.LiftedExpr.Pretty (renderLExpr, renderLiftedLambda)
import Zayin.Literals
import Zayin.Macros (expandMacros)
import Zayin.Parser (parseProgram) -- your top-level parser
import Zayin.Transforms (toFExprM)

--------------------------------------------------------------------------------
-- Command-line Options
--------------------------------------------------------------------------------

-- Your original command type remains (Run vs Compile with an output file)
data Cmd = Run | Compile {output :: FilePath}
  deriving (Show)

-- Extend Options to include a required positional argument for the source file.
data Options = Options
  { cmd :: Cmd,
    sourceFile :: FilePath, -- new: path to the .zai source file
    debug :: Bool,
    keepTmpdir :: Bool
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
    )

optionsParser :: Parser Options
optionsParser =
  Options
    <$> cmdParser
    <*> argument
      str
      ( metavar "SOURCE"
          <> help "Input source file (.zai)"
      )
    <*> switch (long "debug" <> help "Enable debug output")
    <*> switch (long "keep-tmpdir" <> help "Keep temporary build directory")

optsInfo :: ParserInfo Options
optsInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Compile or run the Zayin program"
        <> header "zayin - A Zayin compiler"
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
    -- ("queue_thunk.h", $(makeRelativeToProject "runtime/queue_thunk.h" >>= embedFile)),
    -- ("scheduler.h", $(makeRelativeToProject "runtime/scheduler.h" >>= embedFile)),
    -- ("scheduler.c", $(makeRelativeToProject "runtime/scheduler.c" >>= embedFile)),
    -- ("thread_context.h", $(makeRelativeToProject "runtime/thread_context.h" >>= embedFile)),
    -- ("thread_context.c", $(makeRelativeToProject "runtime/thread_context.c" >>= embedFile)),
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

--------------------------------------------------------------------------------
-- Main Compilation/Execution Pipeline
--------------------------------------------------------------------------------

type CompilationM = LoggingT IO

runCompilation :: CompilationM a -> IO a
runCompilation = runStderrLoggingT

main :: IO ()
main = runCompilation $ do
  opts <- liftIO $ execParser optsInfo
  logInfoN "Starting compilation process..."

  -- Read source file
  src <- liftIO $ TIO.readFile (sourceFile opts)
  logInfoN $ "\nSource being compiled:\n" <> src

  -- Hardcoded AST (uncomment to test compilation without parsing involved)
  -- let
  --   parsedAST =
        -- EApp
        --   (ELam [] (ExprBody
        --     { bodyExprs = []
        --     , finalExpr = EApp (EBuiltinIdent "display") [EApp (EBuiltinIdent "+") [ELit (LInt 1), ELit(LInt 1)]]
        --     }))
        --   []

  -- let
  --   wrappedExpr =
  --     EApp
  --       (ELam [] (ExprBody
  --         { bodyExprs = [ Def "addOne" (ELam ["x"] (ExprBody [] (EApp (EBuiltinIdent "+") [EVar "x", ELit (LInt 1)]))) ]
  --         , finalExpr = EApp (EVar "addOne") [ELit (LInt 1)]
  --         }))
  --       []

  -- Example AST with macro demonstration
  -- let macroExample =
  --       EApp
  --         ( ELam
  --             []
  --             ( ExprBody
  --                 { bodyExprs =
  --                     [ -- Macro definition
  --                       Def "macro" $
  --                         ELam
  --                           ["unless"]
  --                           ( ExprBody [] $
  --                               ELam
  --                                 ["$1", "$2"]
  --                                 ( ExprBody [] $
  --                                     EIf
  --                                       (EApp (EBuiltinIdent "not") [EVar "$1"])
  --                                       (EVar "$2")
  --                                       (ELit LNil)
  --                                 )
  --                           )
  --                     ],
  --                   finalExpr = EApp (EVar "unless") [ELit (LInt 1), ELit (LInt 2)]
  --                 }
  --             )
  --         )
  --         [] -- Empty argument list for CPS wrapper
  -- logInfoN "\nOriginal AST with macro:"
  -- logDebugN $ T.pack (renderExpr macroExample)

  parsedEither <- liftIO $ parseProgram src
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

  -- Generate build directory
  buildDir <- liftIO generateBuildDir
  let cCode = generateC rootStmts protos decls
      fullSource = generateProgramSource cCode

  logInfoN $ "\nGenerated C Code: " <> fullSource

  -- Write and compile C code
  liftIO $ insertSourceIntoBuildDir buildDir fullSource
  makeResult <- liftIO $ invokeMake buildDir

  case makeResult of
    Left errMsg -> do
      logErrorN $ "\nFailed compiling generated C code: " <> T.pack errMsg
      liftIO exitFailure
    Right stdoutStr -> do
      logInfoN "\nCompilation result:"
      when (debug opts) $ logDebugN (T.pack stdoutStr)

  -- Handle output
  case cmd opts of
    Compile {output = out} -> do
      liftIO $ copyBinary buildDir out `catch` handlerCopy
      logInfoN $ "Compiled binary copied to " <> T.pack out
    Run -> do
      let exePath = buildDir </> "compiled_result"
      logInfoN $ "Running " <> T.pack exePath
      liftIO $ callProcess exePath []

  -- Cleanup
  unless (keepTmpdir opts) $ do
    liftIO $ removeDirectoryRecursive buildDir
    logInfoN "Temporary build directory removed."
  where
    handlerCopy :: SomeException -> IO ()
    handlerCopy _ = error "failed copying compiled binary"
