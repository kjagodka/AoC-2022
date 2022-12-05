import Data.Functor ((<&>))
import qualified Day01 (solve)
import qualified Day02 (solve)
import qualified Day03 (solve)
import qualified Day04 (solve)
import qualified Day05 (solve)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

testData :: [(String, String -> IO (String, String), String, (String, String))]
testData =
  [ ( "Day01",
      Day01.solve,
      "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000",
      ("24000", "45000")
    ),
    ( "Day02",
      Day02.solve,
      "A Y\nB X\nC Z",
      ("15", "12")
    ),
    ( "Day03",
      Day03.solve,
      "vJrwpWtwJgWrhcsFMMfFFhFp\n\
      \jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
      \PmmdzqPrVvPwwTWBwg\n\
      \wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
      \ttgJtRGJQctTZtZT\n\
      \CrZsJsPPZsGzwwsLwLmpwMDw\n",
      ("157", "70")
    ),
    ( "Day04",
      Day04.solve,
      "2-4,6-8\n\
      \2-3,4-5\n\
      \5-7,7-9\n\
      \2-8,3-7\n\
      \6-6,4-6\n\
      \2-6,4-8\n",
      ("2", "4")
    ),
    ( "Day05",
      Day05.solve,
      "    [D]    \n\
      \[N] [C]    \n\
      \[Z] [M] [P]\n\
      \ 1   2   3 \n\
      \\n\
      \move 1 from 2 to 1\n\
      \move 3 from 1 to 3\n\
      \move 2 from 2 to 1\n\
      \move 1 from 1 to 2\n",
      ("CMZ", "MCD")
      )
  ]

generateDayTests :: (String, String -> IO (String, String), String, (String, String)) -> IO [TestTree]
generateDayTests (name, fun, input, expected) = do
  output <- fun input
  let test1 = testCase (name ++ " part1") $ assertEqual "" (fst expected) (fst output)
      test2 = testCase (name ++ " part2") $ assertEqual "" (snd expected) (snd output)
   in return [test1, test2]

main :: IO ()
main = do
  tests <- mapM generateDayTests testData <&> (testGroup "tests" . concat)
  defaultMain tests
