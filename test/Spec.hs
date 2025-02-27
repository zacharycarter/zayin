import Control.Exception (bracket, finally)
import Data.List (isInfixOf)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hClose, hGetContents)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (StdStream (..), callProcess, createProcess, proc, readProcessWithExitCode, std_err, std_out, waitForProcess)
import Test.Hspec

-- Helper for compilation that captures and displays output
compileWithOutput :: FilePath -> FilePath -> IO ()
compileWithOutput binaryFile srcFile = do
  putStrLn "\n=== Starting compilation ==="
  putStrLn $ "Compiling " ++ srcFile ++ " to " ++ binaryFile

  (exitCode, stdout, stderr) <-
    readProcessWithExitCode
      "zayin-exe"
      ["compile", "--output", binaryFile, srcFile, "--debug"]
      ""

  putStrLn $ "Compilation stdout:\n" ++ stdout
  putStrLn $ "Compilation stderr:\n" ++ stderr

  case exitCode of
    ExitSuccess -> putStrLn "Compilation succeeded"
    ExitFailure code -> do
      putStrLn $ "Compilation failed with exit code: " ++ show code
      error $
        "Compilation failed with exit code: "
          ++ show code
          ++ "\nstdout: "
          ++ stdout
          ++ "\nstderr: "
          ++ stderr

main :: IO ()
main = hspec $ do
  describe "Compiler End-to-End Tests" $ do
    it "hello world" $ do
      withSystemTempDirectory "zayin-test" $ \tmpDir -> do
        let srcFile = tmpDir </> "arith.zyn"
            binaryFile = tmpDir </> "a.out"
            -- A simple program that displays "Hello, World!"
            source = "display \"Hello, World!\""
        writeFile srcFile source
        -- Invoke the compiler in compile mode, specifying the output binary.
        compileWithOutput binaryFile srcFile
        exists <- doesFileExist binaryFile
        exists `shouldBe` True
        -- Run the produced binary and capture its output.
        (exitCode, output, _) <- readProcessWithExitCode binaryFile [] ""
        exitCode `shouldBe` ExitSuccess
        output `shouldSatisfy` (\out -> "Hello, World!" `isInfixOf` out)

    it "addition" $ do
      withSystemTempDirectory "zayin-test" $ \tmpDir -> do
        let srcFile = tmpDir </> "arith.zyn"
            binaryFile = tmpDir </> "a.out"
            -- A simple program that displays the sum of 2 and 3.
            source = "display(2 + 3)"
        writeFile srcFile source
        -- Invoke the compiler in compile mode, specifying the output binary.
        compileWithOutput binaryFile srcFile
        exists <- doesFileExist binaryFile
        exists `shouldBe` True
        -- Run the produced binary and capture its output.
        (exitCode, output, _) <- readProcessWithExitCode binaryFile [] ""
        exitCode `shouldBe` ExitSuccess
        output `shouldSatisfy` (\out -> "5" `isInfixOf` out)

    it "let binding" $ do
      withSystemTempDirectory "zayin-test" $ \tmpDir -> do
        let srcFile = tmpDir </> "let.zyn"
            binaryFile = tmpDir </> "a.out"
            source = "let x = 5\ndisplay x"
        writeFile srcFile source
        -- Invoke the compiler in compile mode, specifying the output binary.
        compileWithOutput binaryFile srcFile
        exists <- doesFileExist binaryFile
        exists `shouldBe` True
        -- Run the produced binary and capture its output.
        (exitCode, output, _) <- readProcessWithExitCode binaryFile [] ""
        exitCode `shouldBe` ExitSuccess
        output `shouldSatisfy` (\out -> "5" `isInfixOf` out)

    it "conditional" $ do
      withSystemTempDirectory "zayin-test" $ \tmpDir -> do
        let srcFile = tmpDir </> "if.zyn"
            binaryFile = tmpDir </> "a.out"
            -- This program uses an if-expression.
            -- Here we assume that a non-nil value is truthy and nil is falsey.
            -- For example, testing that 0 (or nil) is treated as false.
            source = "if nil: display true; display false"
        writeFile srcFile source
        compileWithOutput binaryFile srcFile
        exists <- doesFileExist binaryFile
        exists `shouldBe` True
        (exitCode, output, _) <- readProcessWithExitCode binaryFile [] ""
        exitCode `shouldBe` ExitSuccess
        output `shouldSatisfy` (\out -> "false" `isInfixOf` out)

    it "lambda" $ do
      withSystemTempDirectory "zayin-test" $ \tmpDir -> do
        let srcFile = tmpDir </> "lambda.zyn"
            binaryFile = tmpDir </> "a.out"
            -- This program defines an anonymous lambda that adds 1 to its argument.
            source = "(fn (x): display(x + 1))(4)"
        writeFile srcFile source
        compileWithOutput binaryFile srcFile
        exists <- doesFileExist binaryFile
        exists `shouldBe` True
        (exitCode, output, _) <- readProcessWithExitCode binaryFile [] ""
        exitCode `shouldBe` ExitSuccess
        output `shouldSatisfy` (\out -> "5" `isInfixOf` out)

    it "macro" $ do
      withSystemTempDirectory "zayin-test" $ \tmpDir -> do
        let srcFile = tmpDir </> "macro.zyn"
            binaryFile = tmpDir </> "a.out"
            -- This program defines a simple macro (here called 'unless') and uses it.
            -- Adjust the syntax as needed to match your language's rules.
            source =
              unlines
                [ "macro unless(a, b):",
                  "  if not a: b else: nil",
                  "",
                  "unless(false): display(\"macro works!\")"
                ]
        writeFile srcFile source
        compileWithOutput binaryFile srcFile
        exists <- doesFileExist binaryFile
        exists `shouldBe` True
        (exitCode, output, _) <- readProcessWithExitCode binaryFile [] ""
        exitCode `shouldBe` ExitSuccess
        output `shouldSatisfy` (\out -> "macro works!" `isInfixOf` out)
