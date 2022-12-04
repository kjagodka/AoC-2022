module Day04 (solve) where

import Data.Functor ((<&>))
import Data.List.Split (splitOn)
import Utils (applyTuple, parseInt)

type Assignment = (Int, Int)

parse :: String -> IO [(Assignment, Assignment)]
parse s = mapM parseLine . lines $ s
  where
    parseAssignment str = case splitOn ['-'] str of
      [begin, end] -> do
        begin' <- parseInt begin
        end' <- parseInt end
        if begin' <= end'
          then return (begin', end')
          else fail $ "Wrong ordering of assignment: " ++ str
      _ -> fail $ "Could not parse assignment: " ++ s
    parseLine line = case splitOn [','] line of
      [a, b] -> do
        a' <- parseAssignment a
        b' <- parseAssignment b
        return (a', b')
      _ -> fail $ "Could not parse line: " ++ line

doContain :: Assignment -> Assignment -> Bool
doContain a b = isInside a b || isInside b a
  where
    isInside (aBegin, aEnd) (bBegin, bEnd) =
      (aBegin >= bBegin) && (aEnd <= bEnd)

part1 :: [(Assignment, Assignment)] -> Int
part1 = length . filter (uncurry doContain)

part2 :: [(Assignment, Assignment)] -> Int
part2 = part1

solve :: String -> IO (Int, Int)
solve input = parse input <&> applyTuple (part1, part2)
