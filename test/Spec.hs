import Control.Exception (bracket)
import Data.List (isInfixOf)
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess, readProcessWithExitCode)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Compiler End-to-End Tests" $ do
    it "compiles and runs an arithmetic program" $ do
      withSystemTempDirectory "zayin-test" $ \tmpDir -> do
        let srcFile    = tmpDir </> "arith.zyn"
            binaryFile = tmpDir </> "a.out"
            -- A simple program that displays the sum of 2 and 3.
            source     = "display(2 + 3)"
        writeFile srcFile source
        -- Invoke the compiler in compile mode, specifying the output binary.
        callProcess "zayin-exe" ["compile", "--output", binaryFile, srcFile]
        exists <- doesFileExist binaryFile
        exists `shouldBe` True
        -- Run the produced binary and capture its output.
        (exitCode, output, _) <- readProcessWithExitCode binaryFile [] ""
        exitCode `shouldBe` ExitSuccess
        output `shouldSatisfy` (\out -> "5" `isInfixOf` out)

    it "compiles and runs a conditional program" $ do
      withSystemTempDirectory "zayin-test" $ \tmpDir -> do
        let srcFile    = tmpDir </> "if.zyn"
            binaryFile = tmpDir </> "a.out"
            -- This program uses an if-expression.
            -- Here we assume that a non-nil value is truthy and nil is falsey.
            -- For example, testing that 0 (or nil) is treated as false.
            source     = "if nil: display true; display false"
        writeFile srcFile source
        callProcess "zayin-exe" ["compile", "--output", binaryFile, srcFile]
        exists <- doesFileExist binaryFile
        exists `shouldBe` True
        (exitCode, output, _) <- readProcessWithExitCode binaryFile [] ""
        exitCode `shouldBe` ExitSuccess
        output `shouldSatisfy` (\out -> "false" `isInfixOf` out)

    it "compiles and runs a lambda function program" $ do
      withSystemTempDirectory "zayin-test" $ \tmpDir -> do
        let srcFile    = tmpDir </> "lambda.zyn"
            binaryFile = tmpDir </> "a.out"
            -- This program defines an anonymous lambda that adds 1 to its argument.
            source     = "(fn (x): display(x + 1))(4)"
        writeFile srcFile source
        callProcess "zayin-exe" ["compile", "--output", binaryFile, srcFile]
        exists <- doesFileExist binaryFile
        exists `shouldBe` True
        (exitCode, output, _) <- readProcessWithExitCode binaryFile [] ""
        exitCode `shouldBe` ExitSuccess
        output `shouldSatisfy` (\out -> "5" `isInfixOf` out)

    it "compiles and runs a program with macro expansion" $ do
      withSystemTempDirectory "zayin-test" $ \tmpDir -> do
        let srcFile    = tmpDir </> "macro.zyn"
            binaryFile = tmpDir </> "a.out"
            -- This program defines a simple macro (here called 'unless') and uses it.
            -- Adjust the syntax as needed to match your language's rules.
            source     = unlines
              [ "macro unless(a, b):"
              , "  if not a: b else: nil"
              , ""
              , "unless(false): display(\"macro works!\")"
              ]
        writeFile srcFile source
        callProcess "zayin-exe" ["compile", "--output", binaryFile, srcFile]
        exists <- doesFileExist binaryFile
        exists `shouldBe` True
        (exitCode, output, _) <- readProcessWithExitCode binaryFile [] ""
        exitCode `shouldBe` ExitSuccess
        output `shouldSatisfy` (\out -> "macro works!" `isInfixOf` out)
