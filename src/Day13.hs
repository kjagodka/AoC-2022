module Day13 (solve) where

import Data.Char (isDigit, isSpace)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Utils (applyTuple, pairMap)

data Packet = Number Int | List [Packet]

instance Read Packet where
  readsPrec precedence str
    | null str = []
    | isSpace $ head str = readsPrec precedence (tail str)
    | isDigit $ head str =
      let (n, rest) = head $ readsPrec precedence str :: (Int, String)
       in [(Number n, rest)]
    | (== '[') $ head str =
      let (packets, rest) = head $ readsPrec precedence str :: ([Packet], String)
       in [(List packets, rest)]
    | otherwise = []

instance Show Packet where
  show (Number n) = show n
  show (List list) = show list

instance Eq Packet where
  (Number n) == (Number m) = n == m
  (Number n) == (List l) = [Number n] == l
  (List l) == (Number n) = l == [Number n]
  (List l1) == (List l2) = l1 == l2

instance Ord Packet where
  (Number n) `compare` (Number m) = n `compare` m
  (Number n) `compare` (List l) = [Number n] `compare` l
  (List l) `compare` (Number n) = l `compare` [Number n]
  (List l1) `compare` (List l2) = l1 `compare` l2

parse :: MonadFail m => String -> m [(Packet, Packet)]
parse = mapM parsePair . splitOn [""] . lines
  where
    parsePair :: MonadFail m => [String] -> m (Packet, Packet)
    parsePair [a, b] =
      let a' = readMaybe a :: Maybe Packet
          b' = readMaybe b :: Maybe Packet
       in case (a', b') of
            (Just a'', Just b'') -> return (a'', b'')
            (Nothing, _) -> fail $ "Could not parse packet: " ++ a
            (_, Nothing) -> fail $ "Could not parse packet: " ++ b
    parsePair lns = fail $ "Wrong number of lines, expected pair of packets in:\n" ++ unlines lns

part1 :: [(Packet, Packet)] -> Int
part1 = sum . map fst . filter (uncurry (<=) . snd) . zip [1 ..]

part2 :: [(Packet, Packet)] -> Int
part2 = part1

solve :: MonadFail m => String -> m (String, String)
solve input = pairMap show . applyTuple (part1, part2) <$> parse input