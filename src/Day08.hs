module Day08 (solve) where

import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.List (mapAccumL, mapAccumR, transpose)
import Utils (applyTuple, pairMap)

parse :: String -> IO [[Int]]
parse str = do
  result <- mapM parseLine . lines $ str
  if and . zipWith (\a b -> length a == length b) result $ tail result
    then return result
    else fail $ "Error, not all tree rows have same height in input:\n" ++ str
  where
    parseLine :: String -> IO [Int]
    parseLine = mapM parseChar

    parseChar :: Char -> IO Int
    parseChar c =
      if isDigit c
        then return $ fromEnum c - fromEnum '0'
        else fail $ "Could not parse character: '" ++ [c] ++ "' as tree height"

visible :: [[Int]] -> [[Bool]]
visible trees =
  let left = visibleLeft trees
      right = visibleRight trees
      up = transpose . visibleLeft . transpose $ trees
      down = transpose . visibleRight . transpose $ trees
   in foldl1 (zipWith (zipWith (||))) [left, right, up, down]
  where
    visibleLeft :: [[Int]] -> [[Bool]]
    visibleLeft = map (snd . mapAccumL (\acc tree -> (max tree acc, tree > acc)) (-1))
    visibleRight :: [[Int]] -> [[Bool]]
    visibleRight = map (snd . mapAccumR (\acc tree -> (max tree acc, tree > acc)) (-1))

score :: [[Int]] -> [[Int]]
score trees =
  let left = rangeLeft trees
      right = rangeRight trees
      up = transpose . rangeLeft . transpose $ trees
      down = transpose . rangeRight . transpose $ trees
   in foldl1 (zipWith (zipWith (*))) [left, right, up, down]
  where
    helper :: ([(Int, Int)], Int) -> Int -> (([(Int, Int)], Int), Int)
    helper (tallestBefore, index) height =
      let tallestBefore' = dropWhile ((< height) . fst) tallestBefore
       in (((height, index) : tallestBefore', index + 1), index - snd (head tallestBefore'))
    rangeLeft :: [[Int]] -> [[Int]]
    rangeLeft = map (snd . mapAccumL helper ([(maxBound, 0)], 0))
    rangeRight :: [[Int]] -> [[Int]]
    rangeRight = map (snd . mapAccumR helper ([(maxBound, 0)], 0))

part1 :: [[Int]] -> Int
part1 = sum . map (length . filter id) . visible

part2 :: [[Int]] -> Int
part2 = maximum . map maximum . score

solve :: String -> IO (String, String)
solve input = parse input <&> applyTuple (part1, part2) <&> pairMap show