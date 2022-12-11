module Day01 (solve) where

import Data.List (sort)
import Data.List.Split (splitOn)
import Utils (applyTuple, pairMap, parseInt)

parse :: String -> [[Int]]
parse = map (map parseInt) . splitOn [""] . lines

part1 :: [[Int]] -> Int
part1 = maximum . map sum

part2 :: [[Int]] -> Int
part2 = sum . take 3 . reverse . sort . map sum

solve :: String -> (String, String)
solve = pairMap show . applyTuple (part1, part2) . parse
