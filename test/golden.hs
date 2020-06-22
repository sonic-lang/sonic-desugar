import           System.Directory               ( listDirectory )
import           System.FilePath.Posix          ( isExtensionOf )
import           Data.String.Conversions        ( convertString )
import           Text.Pretty.Simple             ( pShowNoColor )

import           Test.Tasty
import           Test.Tasty.Golden              ( goldenVsStringDiff )

import           Language.Sonic.Parser          ( parseModule )
import           Language.Sonic.Syntax.Location ( noLoc )
import           Language.Sonic.Compiler.Desugar
                                                ( desugarModule )
import           Language.Sonic.Compiler.Desugar.IR.Instance
                                                ( )

import           TestImport                     ( runTestDesugar )


goldenDir :: FilePath
goldenDir = "test/golden/"

main :: IO ()
main = do
  tests <- goldenTest
  defaultMain $ testGroup "golden" tests

goldenTest :: IO [TestTree]
goldenTest = do
  files <- listDirectory goldenDir
  let sources = filter (isExtensionOf ".sonic") files
  pure $ map makeGoldenTest sources

makeGoldenTest :: FilePath -> TestTree
makeGoldenTest file = goldenVsStringDiff file diff goldenPath $ do
  source <- readFile path
  syn    <- case parseModule source of
    Left  e -> fail (show e)
    Right x -> pure (noLoc x)
  let result = runTestDesugar path (desugarModule syn)
  pure $ convertString (pShowNoColor result)
 where
  diff ref new = ["diff", "-y", ref, new]
  goldenPath = path ++ ".golden"
  path       = goldenDir ++ file
