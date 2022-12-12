module Day06 (solve) where

import Data.Char (isLower)
import Data.List (findIndex)
import Data.List.Split (divvy)
import Data.Set (fromList, size)
import Utils (applyTuple, joinPair, pairMap)

parse :: MonadFail m => String -> m String
parse str = case lines str of
  [line] -> mapM verifyChar line
  _ -> fail $ "Expected exactly one line, could not parse input: " ++ str
  where
    verifyChar c = if isLower c then return c else fail $ "Encountered illegal character: '" ++ [c] ++ "' when parsing input:" ++ str

isMarker :: [Char] -> Bool
isMarker str = length str == size (fromList str)

part1 :: MonadFail m => String -> m Int
part1 s = case findIndex isMarker . divvy 4 1 $ s of
  Just n -> return $ n + 4
  Nothing -> fail $ "No marker in input: " ++ s

part2 :: MonadFail m => String -> m Int
part2 s = case findIndex isMarker . divvy 14 1 $ s of
  Just n -> return $ n + 14
  Nothing -> fail $ "No marker in input: " ++ s

solve :: MonadFail m => String -> m (String, String)
solve input = fmap (pairMap show) $ parse input >>= joinPair . applyTuple (part1, part2)
