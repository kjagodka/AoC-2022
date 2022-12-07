import Control.Exception (try)
import Data.Functor ((<&>))
import qualified Day01 (solve)
import qualified Day02 (solve)
import qualified Day03 (solve)
import qualified Day04 (solve)
import qualified Day05 (solve)
import qualified Day06 (solve)
import qualified Day07 (solve)
import System.IO.Error (ioeGetErrorString)
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
    ),
    ( "Day06 example1",
      Day06.solve,
      "mjqjpqmgbljsphdztnvjfqwrcgsmlb\n",
      ("7", "19")
    ),
    ( "Day06 example2",
      Day06.solve,
      "bvwbjplbgvbhsrlpgdmjqwftvncz\n",
      ("5", "23")
    ),
    ( "Day06 example3",
      Day06.solve,
      "nppdvjthqldpwncqszvftbrmjlhg\n",
      ("6", "23")
    ),
    ( "Day06 example4",
      Day06.solve,
      "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg\n",
      ("10", "29")
    ),
    ( "Day06 example5",
      Day06.solve,
      "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw\n",
      ("11", "26")
    ),
    ( "Day07",
      Day07.solve,
      "$ cd /\n\
      \$ ls\n\
      \dir a\n\
      \14848514 b.txt\n\
      \8504156 c.dat\n\
      \dir d\n\
      \$ cd a\n\
      \$ ls\n\
      \dir e\n\
      \29116 f\n\
      \2557 g\n\
      \62596 h.lst\n\
      \$ cd e\n\
      \$ ls\n\    
      \584 i\n\
      \$ cd ..\n\
      \$ cd ..\n\
      \$ cd d\n\
      \$ ls\n\
      \4060174 j\n\
      \8033020 d.log\n\
      \5626152 d.ext\n\
      \7214296 k\n",
      ("95437", "24933642"))
  ]

generateDayTests :: (String, String -> IO (String, String), String, (String, String)) -> IO [TestTree]
generateDayTests (name, fun, input, (exp1, exp2)) = do
  outputsOrError <- try $ fun input :: IO (Either IOError (String, String))
  case outputsOrError of
    Left e -> do
      return $ makeTests (ioeGetErrorString e, ioeGetErrorString e)
    Right outputs -> return $ makeTests outputs
  where
    makeTests (out1, out2) =
      let test1 = testCase (name ++ " part1") $ assertEqual "" exp1 out1
          test2 = testCase (name ++ " part2") $ assertEqual "" exp2 out2
       in [test1, test2]

main :: IO ()
main = do
  tests <- mapM generateDayTests testData <&> testGroup "tests" . concat
  defaultMain tests
