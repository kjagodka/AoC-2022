module Day08 (solve) where

import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.List (find, mapAccumL, mapAccumR, transpose)
import Data.Maybe (fromMaybe)
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
  let left = mapRangeLeft trees
      right = mapRangeRight trees
      up = transpose . mapRangeLeft . transpose $ trees
      down = transpose . mapRangeRight . transpose $ trees
   in foldl1 (zipWith (zipWith (*))) [left, right, up, down]
  where
    mapRangeLeft :: [[Int]] -> [[Int]]
    mapRangeLeft = map (snd . mapAccumL viewRange ([], 0))

    viewRange :: ([(Int, Int)], Int) -> Int -> (([(Int, Int)], Int), Int)
    viewRange (tallestBefore, index) height = ((newTallestBefore, index + 1), index - lastTallerIndex)
      where
        newTallestBefore = (height, index) : dropWhile ((< height) . fst) tallestBefore
        lastTallerIndex = snd . fromMaybe (0, 0) $ find ((>= height) . fst) tallestBefore

    mapRangeRight :: [[Int]] -> [[Int]]
    mapRangeRight = map reverse . mapRangeLeft . map reverse

part1 :: [[Int]] -> Int
part1 = sum . map (length . filter id) . visible

part2 :: [[Int]] -> Int
part2 = maximum . map maximum . score

solve :: String -> IO (String, String)
solve input = parse input <&> applyTuple (part1, part2) <&> pairMap show