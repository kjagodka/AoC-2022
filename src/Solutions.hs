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
import qualified Day10 (solve)
import qualified Day11 (solve)

solvedDays :: [Int]
solvedDays = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]

solve :: MonadFail m => Int -> String -> m (String, String)
solve 1 = Day01.solve
solve 2 = Day02.solve
solve 3 = Day03.solve
solve 4 = Day04.solve
solve 5 = Day05.solve
solve 6 = Day06.solve
solve 7 = Day07.solve
solve 8 = Day08.solve
solve 9 = Day09.solve
solve 10 = Day10.solve
solve 11 = Day11.solve
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
      "R 5\n\
      \U 8\n\
      \L 8\n\
      \D 3\n\
      \R 17\n\
      \D 10\n\
      \L 25\n\
      \U 20\n",
      ("88", "36")
    ),
    ( 10,
      "addx 15\n\
      \addx -11\n\
      \addx 6\n\
      \addx -3\n\
      \addx 5\n\
      \addx -1\n\
      \addx -8\n\
      \addx 13\n\
      \addx 4\n\
      \noop\n\
      \addx -1\n\
      \addx 5\n\
      \addx -1\n\
      \addx 5\n\
      \addx -1\n\
      \addx 5\n\
      \addx -1\n\
      \addx 5\n\
      \addx -1\n\
      \addx -35\n\
      \addx 1\n\
      \addx 24\n\
      \addx -19\n\
      \addx 1\n\
      \addx 16\n\
      \addx -11\n\
      \noop\n\
      \noop\n\
      \addx 21\n\
      \addx -15\n\
      \noop\n\
      \noop\n\
      \addx -3\n\
      \addx 9\n\
      \addx 1\n\
      \addx -3\n\
      \addx 8\n\
      \addx 1\n\
      \addx 5\n\
      \noop\n\
      \noop\n\
      \noop\n\
      \noop\n\
      \noop\n\
      \addx -36\n\
      \noop\n\
      \addx 1\n\
      \addx 7\n\
      \noop\n\
      \noop\n\
      \noop\n\
      \addx 2\n\
      \addx 6\n\
      \noop\n\
      \noop\n\
      \noop\n\
      \noop\n\
      \noop\n\
      \addx 1\n\
      \noop\n\
      \noop\n\
      \addx 7\n\
      \addx 1\n\
      \noop\n\
      \addx -13\n\
      \addx 13\n\
      \addx 7\n\
      \noop\n\
      \addx 1\n\
      \addx -33\n\
      \noop\n\
      \noop\n\
      \noop\n\
      \addx 2\n\
      \noop\n\
      \noop\n\
      \noop\n\
      \addx 8\n\
      \noop\n\
      \addx -1\n\
      \addx 2\n\
      \addx 1\n\
      \noop\n\
      \addx 17\n\
      \addx -9\n\
      \addx 1\n\
      \addx 1\n\
      \addx -3\n\
      \addx 11\n\
      \noop\n\
      \noop\n\
      \addx 1\n\
      \noop\n\
      \addx 1\n\
      \noop\n\
      \noop\n\
      \addx -13\n\
      \addx -19\n\
      \addx 1\n\
      \addx 3\n\
      \addx 26\n\
      \addx -30\n\
      \addx 12\n\
      \addx -1\n\
      \addx 3\n\
      \addx 1\n\
      \noop\n\
      \noop\n\
      \noop\n\
      \addx -9\n\
      \addx 18\n\
      \addx 1\n\
      \addx 2\n\
      \noop\n\
      \noop\n\
      \addx 9\n\
      \noop\n\
      \noop\n\
      \noop\n\
      \addx -1\n\
      \addx 2\n\
      \addx -37\n\
      \addx 1\n\
      \addx 3\n\
      \noop\n\
      \addx 15\n\
      \addx -21\n\
      \addx 22\n\
      \addx -6\n\
      \addx 1\n\
      \noop\n\
      \addx 2\n\
      \addx 1\n\
      \noop\n\
      \addx -10\n\
      \noop\n\
      \noop\n\
      \addx 20\n\
      \addx 1\n\
      \addx 2\n\
      \addx 2\n\
      \addx -6\n\
      \addx -11\n\
      \noop\n\
      \noop",
      ( "13140",
        "\n\
        \##..##..##..##..##..##..##..##..##..##..\n\
        \###...###...###...###...###...###...###.\n\
        \####....####....####....####....####....\n\
        \#####.....#####.....#####.....#####.....\n\
        \######......######......######......####\n\
        \#######.......#######.......#######.....\n"
      )
    ),
    ( 11,
      "Monkey 0:\n\
      \  Starting items: 79, 98\n\
      \  Operation: new = old * 19\n\
      \  Test: divisible by 23\n\
      \    If true: throw to monkey 2\n\
      \    If false: throw to monkey 3\n\
      \\n\
      \Monkey 1:\n\
      \  Starting items: 54, 65, 75, 74\n\
      \  Operation: new = old + 6\n\
      \  Test: divisible by 19\n\
      \    If true: throw to monkey 2\n\
      \    If false: throw to monkey 0\n\
      \\n\
      \Monkey 2:\n\
      \  Starting items: 79, 60, 97\n\
      \  Operation: new = old * old\n\
      \  Test: divisible by 13\n\
      \    If true: throw to monkey 1\n\
      \    If false: throw to monkey 3\n\
      \\n\
      \Monkey 3:\n\
      \  Starting items: 74\n\
      \  Operation: new = old + 3\n\
      \  Test: divisible by 17\n\
      \    If true: throw to monkey 0\n\
      \    If false: throw to monkey 1\n",
      ("10605", "2713310158")
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
    (8, ("1818", "368368")),
    (9, ("6197", "2562")),
    (10,
      ( "17180",
        "\n\
        \###..####.#..#.###..###..#....#..#.###..\n\
        \#..#.#....#..#.#..#.#..#.#....#..#.#..#.\n\
        \#..#.###..####.#..#.#..#.#....#..#.###..\n\
        \###..#....#..#.###..###..#....#..#.#..#.\n\
        \#.#..#....#..#.#....#.#..#....#..#.#..#.\n\
        \#..#.####.#..#.#....#..#.####..##..###..\n"
      )
    ),
    (11, ("95472", "17926061332"))
  ]