module Day09 (solve) where

import Data.Functor ((<&>))
import Data.Set (fromList, size)
import Utils (applyTuple, pairMap, parseInt)

data Move = MUp | MDown | MLeft | MRight

type Knot = (Int, Int)

type Rope = [Knot] --(head, tail)

parse :: String -> IO [Move]
parse = fmap concat . mapM parseLine . lines
  where
    parseLine line = case words line of
      [dir, n] -> do
        dir' <- parseMove dir
        n' <- parseInt n
        return $ replicate n' dir'
      _ -> fail $ "Could not parse line: " ++ line

    parseMove "U" = return MUp
    parseMove "D" = return MDown
    parseMove "L" = return MLeft
    parseMove "R" = return MRight
    parseMove mv = fail $ "Could not parse move: " ++ mv

applyMove :: Rope -> Move -> Rope
applyMove (h:t) m = result
  where
    result = moveHead h m : zipWith follow t result

    moveHead :: Knot -> Move -> Knot
    moveHead (hx, hy) MUp = (hx, hy + 1)
    moveHead (hx, hy) MDown = (hx, hy - 1)
    moveHead (hx, hy) MLeft = (hx - 1, hy)
    moveHead (hx, hy) MRight = (hx + 1, hy)

    follow :: Knot -> Knot -> Knot
    follow (tx, ty) (hx, hy)
      | areTouching (tx, ty) (hx, hy) = (tx, ty)
      | tx == hx = (tx, towards ty hy)
      | ty == hy = (towards tx hx, ty)
      | otherwise = (towards tx hx, towards ty hy)

    areTouching :: Knot -> Knot -> Bool
    areTouching (tx, ty) (hx, hy) = max (abs (hx - tx)) (abs (hy - ty)) < 2

    towards :: Int -> Int -> Int
    towards from to = from + signum (to - from)
applyMove [] _ = []

simulateRope :: Rope -> [Move] -> [Rope]
simulateRope initial moves = positions
  where
    positions = initial : zipWith applyMove positions moves

countTailPositions :: [Rope] -> Int
countTailPositions = size . fromList . map last

part1 :: [Move] -> Int
part1 = countTailPositions . simulateRope (replicate 2 (0, 0))

part2 :: [Move] -> Int
part2 = countTailPositions . simulateRope (replicate 10 (0, 0))

solve :: String -> IO (String, String)
solve input = parse input <&> applyTuple (part1, part2) <&> pairMap show