module Day04 (solve) where

import Data.Functor ((<&>))
import Data.List.Split (splitOn)
import Utils (applyTuple, parseInt)

type Assignment = (Int, Int)

parse :: String -> IO [(Assignment, Assignment)]
parse = mapM parseLine . lines
  where
    parseAssignment str = case splitOn ['-'] str of
      [begin, end] -> do
        begin' <- parseInt begin
        end' <- parseInt end
        if begin' <= end'
          then return (begin', end')
          else fail $ "Wrong ordering of assignment: " ++ str
      _ -> fail $ "Could not parse assignment: " ++ str
    parseLine line = case splitOn [','] line of
      [a, b] -> do
        a' <- parseAssignment a
        b' <- parseAssignment b
        return (a', b')
      _ -> fail $ "Could not parse line: " ++ line

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

solve :: String -> IO (Int, Int)
solve input = parse input <&> applyTuple (part1, part2)
