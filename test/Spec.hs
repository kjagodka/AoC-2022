import           Data.Functor     ((<&>))
import qualified Day01            (solve)
import qualified Day02            (solve)
import           Test.Tasty       (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)

testData :: [(String, String -> IO (Int, Int), String, (Int, Int))]
testData = [("Day01",
              Day01.solve,
              "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000",
              (24000, 45000)),
            ("Day02",
              Day02.solve,
              "A Y\nB X\nC Z",
              (15, 0))]

generateDayTests :: (String, String -> IO (Int, Int), String, (Int, Int)) -> IO [TestTree]
generateDayTests (name, fun, input, expected) = do
    output <- fun input
    let test1 = testCase (name ++ " part1") $ assertEqual "" (fst expected) (fst output)
        test2 = testCase (name ++ " part2") $ assertEqual "" (snd expected) (snd output)
     in return [test1, test2]

main :: IO ()
main = do
    tests <- mapM generateDayTests testData <&> (testGroup "tests" . concat)
    defaultMain tests


