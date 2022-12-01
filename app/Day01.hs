module Day01 (solve, main) where
import Data.List.Split (splitOn)
import Data.List (sort)

parse :: String -> [[Int]]
parse = map (map read) . splitOn [""] . lines

part1 :: [[Int]] -> Int
part1 = maximum . map sum

part2 :: [[Int]] -> Int
part2 = sum . take 3 . reverse . sort . map sum

solve :: String -> String
solve input = 
    let parsed = parse input 
    in "Part1: " ++ show (part1 parsed) ++ "\nPart2: " ++ show (part2 parsed) ++ "\n"

main :: IO ()
main = interact solve