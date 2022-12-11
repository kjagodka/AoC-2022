module Day04 (solve) where

import Data.List.Split (splitOn)
import Utils (applyTuple, pairMap, parseInt)

type Assignment = (Int, Int)

parse :: String -> [(Assignment, Assignment)]
parse = map parseLine . lines
  where
    parseAssignment str = case splitOn ['-'] str of
      [begin, end] ->
        let begin' = parseInt begin
            end' = parseInt end
         in if begin' <= end'
              then (begin', end')
              else error $ "Wrong ordering of assignment: " ++ str
      _ -> error $ "Could not parse assignment: " ++ str
    parseLine line = case splitOn [','] line of
      [a, b] ->
        let a' = parseAssignment a
            b' = parseAssignment b
         in (a', b')
      _ -> error $ "Could not parse line: " ++ line

isContaining :: Assignment -> Assignment -> Bool
isContaining a b = isInside a b || isInside b a
  where
    isInside (aBegin, aEnd) (bBegin, bEnd) =
      (aBegin >= bBegin) && (aEnd <= bEnd)

isOverlapping :: Assignment -> Assignment -> Bool
isOverlapping a b = not $ isSeparate a b
  where
    isSeparate (aBegin, aEnd) (bBegin, bEnd) =
      (aBegin > bEnd) || (aEnd < bBegin)

part1 :: [(Assignment, Assignment)] -> Int
part1 = length . filter (uncurry isContaining)

part2 :: [(Assignment, Assignment)] -> Int
part2 = length . filter (uncurry isOverlapping)

solve :: String -> (String, String)
solve = pairMap show . applyTuple (part1, part2) . parse
