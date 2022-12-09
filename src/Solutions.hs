module Solutions (solve, solvedDays, examplesTestData, fullTestsData) where

import qualified Day01 (solve)
import qualified Day02 (solve)
import qualified Day03 (solve)
import qualified Day04 (solve)
import qualified Day05 (solve)
import qualified Day06 (solve)
import qualified Day07 (solve)
import qualified Day08 (solve)
import qualified Day09 (solve)

solvedDays :: [Int]
solvedDays = [1, 2, 3, 4, 5, 6, 7, 8, 9]

solve :: Int -> String -> IO (String, String)
solve 1 = Day01.solve
solve 2 = Day02.solve
solve 3 = Day03.solve
solve 4 = Day04.solve
solve 5 = Day05.solve
solve 6 = Day06.solve
solve 7 = Day07.solve
solve 8 = Day08.solve
solve 9 = Day09.solve
solve _ = undefined

examplesTestData :: [(Int, String, (String, String))]
examplesTestData =
  [ ( 1,
      "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000",
      ("24000", "45000")
    ),
    ( 2,
      "A Y\nB X\nC Z",
      ("15", "12")
    ),
    ( 3,
      "vJrwpWtwJgWrhcsFMMfFFhFp\n\
      \jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
      \PmmdzqPrVvPwwTWBwg\n\
      \wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
      \ttgJtRGJQctTZtZT\n\
      \CrZsJsPPZsGzwwsLwLmpwMDw\n",
      ("157", "70")
    ),
    ( 4,
      "2-4,6-8\n\
      \2-3,4-5\n\
      \5-7,7-9\n\
      \2-8,3-7\n\
      \6-6,4-6\n\
      \2-6,4-8\n",
      ("2", "4")
    ),
    ( 5,
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
    ( 6,
      "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg\n",
      ("10", "29")
    ),
    ( 7,
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
      ("95437", "24933642")
    ),
    ( 8,
      "30373\n\
      \25512\n\
      \65332\n\
      \33549\n\
      \35390\n",
      ("21", "8")
    ),
    ( 9,
      "R 4\n\
      \U 4\n\
      \L 3\n\
      \D 1\n\
      \R 4\n\
      \D 1\n\
      \L 5\n\
      \R 2\n",
      ("13", "13")
    )
  ]

fullTestsData :: [(Int, (String, String))]
fullTestsData =
  [ (1, ("70296", "205381")),
    (2, ("10595", "9541")),
    (3, ("7997", "2545")),
    (4, ("464", "770")),
    (5, ("ZRLJGSCTR", "PRTTGRFPB")),
    (6, ("1702", "3559")),
    (7, ("1443806", "942298")),
    (8, ("1818", "368368"))
  ]