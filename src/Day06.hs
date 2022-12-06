module Day06 (solve) where

import Data.Char (isLower)
import Data.Functor ((<&>))
import Data.List (elemIndex)
import Data.List.Split (divvy)
import Data.Set (fromList, size)
import Utils (applyTuple, joinPair, pairMap)

parse :: String -> IO String
parse str = case lines str of
  [line] -> mapM verifyChar line
  _ -> fail $ "Expected exactly one line, could not parse input: " ++ str
  where
    verifyChar c = if isLower c then return c else fail $ "Encountered illegal character: '" ++ [c] ++ "' when parsing input:" ++ str

isMarker :: [Char] -> Bool
isMarker str = length str == size (fromList str)

part1 :: String -> IO Int
part1 s = case elemIndex True . map isMarker . divvy 4 1 $ s of
  Just n -> return $ n + 4
  Nothing -> fail $ "No marker in input: " ++ s

part2 :: String -> IO Int
part2 s = case elemIndex True . map isMarker . divvy 14 1 $ s of
  Just n -> return $ n + 14
  Nothing -> fail $ "No marker in input: " ++ s

solve :: String -> IO (String, String)
solve input = parse input >>= joinPair . applyTuple (part1, part2) <&> pairMap show
