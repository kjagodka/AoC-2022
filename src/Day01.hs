module Day01 (solve) where

import Data.Functor ((<&>))
import Data.List (sort)
import Data.List.Split (splitOn)
import Utils (applyTuple, pairMap, parseInt)

parse :: MonadFail m => String -> m [[Int]]
parse = mapM (mapM parseInt) . splitOn [""] . lines

part1 :: [[Int]] -> Int
part1 = maximum . map sum

part2 :: [[Int]] -> Int
part2 = sum . take 3 . reverse . sort . map sum

solve :: MonadFail m => String -> m (String, String)
solve input = parse input <&> applyTuple (part1, part2) <&> pairMap show