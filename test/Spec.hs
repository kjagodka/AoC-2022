import Control.Exception (try)
import Data.Functor ((<&>))
import Solutions (examplesTestData, fullTestsData, solve)
import System.IO.Error (ioeGetErrorString)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Utils (readInputs)

generateTestPair :: String -> IO (String, String) -> (String, String) -> IO [TestTree]
generateTestPair testName ioOutput (exp1, exp2) = do
  outputsOrError <- try ioOutput
  case outputsOrError of
    Left e -> do
      return $ makeTests (ioeGetErrorString e, ioeGetErrorString e)
    Right outputs -> return $ makeTests outputs
  where
    makeTests (out1, out2) =
      let test1 = testCase (testName ++ " part1") $ assertEqual "" exp1 out1
          test2 = testCase (testName ++ " part2") $ assertEqual "" exp2 out2
       in [test1, test2]

main :: IO ()
main = do
  exampleTests <- mapM (\(n, input, expOutput) -> generateTestPair ("Day" ++ show n) (solve n input) expOutput) examplesTestData <&> testGroup "Examples" . concat
  fullTests <- mapM (\(n, expOutput) -> generateTestPair ("Day" ++ show n) (readInputs n >>= solve n) expOutput) fullTestsData <&> testGroup "Full" . concat
  defaultMain $ testGroup "All" [exampleTests, fullTests]