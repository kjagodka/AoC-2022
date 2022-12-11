module Day06 (solve) where

import Data.Char (isLower)
import Data.List (findIndex)
import Data.List.Split (divvy)
import Data.Set (fromList, size)
import Utils (applyTuple, pairMap)

parse :: String -> String
parse str = case lines str of
  [line] -> map verifyChar line
  _ -> error $ "Expected exactly one line, could not parse input: " ++ str
  where
    verifyChar c = if isLower c then c else error $ "Encountered illegal character: '" ++ [c] ++ "' when parsing input:" ++ str

isMarker :: [Char] -> Bool
isMarker str = length str == size (fromList str)

part1 :: String -> Int
part1 s = case findIndex isMarker . divvy 4 1 $ s of
  Just n -> n + 4
  Nothing -> error $ "No marker in input: " ++ s

part2 :: String -> Int
part2 s = case findIndex isMarker . divvy 14 1 $ s of
  Just n -> n + 14
  Nothing -> error $ "No marker in input: " ++ s

solve :: String -> (String, String)
solve = pairMap show . applyTuple (part1, part2) . parse
