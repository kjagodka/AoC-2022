module Day13 (solve) where

import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Utils (applyTuple, pairMap)
import Data.Bifunctor (first)

data Value = Number Int | List [Value]

instance Read Value where
  readsPrec precedence str
    | null str = []
    | isDigit $ head str = map (first Number) $ readsPrec precedence str
    | (== '[') $ head str = map (first List) $ readsPrec precedence str
    | otherwise = []

instance Ord Value where
  (Number n) `compare` (Number m) = n `compare` m
  (Number n) `compare` (List l) = [Number n] `compare` l
  (List l) `compare` (Number n) = l `compare` [Number n]
  (List l1) `compare` (List l2) = l1 `compare` l2

instance Eq Value where
  a == b = (a `compare` b) == EQ

parse :: MonadFail m => String -> m [(Value, Value)]
parse = mapM parsePair . splitOn [""] . lines
  where
    parsePair :: MonadFail m => [String] -> m (Value, Value)
    parsePair [a, b] =
      let a' = readMaybe a :: Maybe Value
          b' = readMaybe b :: Maybe Value
       in case (a', b') of
            (Just a'', Just b'') -> return (a'', b'')
            (Nothing, _) -> fail $ "Could not parse packet: " ++ a
            (_, Nothing) -> fail $ "Could not parse packet: " ++ b
    parsePair lns = fail $ "Wrong number of lines, expected pair of packets in:\n" ++ unlines lns

part1 :: [(Value, Value)] -> Int
part1 = sum . map fst . filter (uncurry (<=) . snd) . zip [1 ..]

part2 :: [(Value, Value)] -> Int
part2 pairs =
  let packets = concatMap (\(a, b) -> [a, b]) $ (read "[[2]]", read "[[6]]") : pairs
      i1 = length . filter (<= read "[[2]]") $ packets
      i2 = length . filter (<= read "[[6]]") $ packets
   in i1 * i2

solve :: MonadFail m => String -> m (String, String)
solve input = pairMap show . applyTuple (part1, part2) <$> parse input