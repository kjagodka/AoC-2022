module Day03 (solve) where

import Data.Char (isLetter, isLower)
import Data.List.Split (chunksOf)
import Data.Set (Set, fromList, intersection, toList, union)
import Utils (applyTuple, pairMap)

verify :: String -> String
verify s =
  if all isLetter s && even (length s)
    then s
    else error $ "Could not parse line" ++ s

split :: [a] -> ([a], [a])
split xs = splitAt n xs
  where
    n = length xs `div` 2

parse :: String -> [(Set Char, Set Char)]
parse = map parseLine . lines
  where
    parseLine = pairMap fromList . split . verify

itemPriority :: Char -> Int
itemPriority c =
  if isLower c
    then fromEnum c - fromEnum 'a' + 1
    else fromEnum c - fromEnum 'A' + 27

part1 :: [(Set Char, Set Char)] -> Int
part1 = sum . map (itemPriority . head . toList . uncurry intersection)

part2 :: [(Set Char, Set Char)] -> Int
part2 = sum . map (itemPriority . head . toList . foldl1 intersection) . chunksOf 3 . map (uncurry union)

solve :: String -> (String, String)
solve = pairMap show . applyTuple (part1, part2) . parse