module Day01 (solve, main) where
import           Data.List       (sort)
import           Data.List.Split (splitOn)
import           Utils           (combineParts)

parse :: String -> [[Int]]
parse = map (map read) . splitOn [""] . lines

part1 :: [[Int]] -> Int
part1 = maximum . map sum

part2 :: [[Int]] -> Int
part2 = sum . take 3 . reverse . sort . map sum

solve :: String -> String
solve = combineParts part1 part2 . parse

main :: IO ()
main = interact solve
