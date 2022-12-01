module Day01 (solve, main) where
import Data.List.Split (splitOn)

parse :: String -> [[Int]]
parse = map (map read) . splitOn [""] . lines

part1 :: [[Int]] -> Int
part1 = maximum . map sum

solve :: String -> String
solve = show . part1 . parse

main :: IO ()
main = interact solve