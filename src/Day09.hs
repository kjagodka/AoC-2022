module Day09 (solve) where

import Data.Functor ((<&>))
import Data.Set (fromList, size)
import Utils (applyTuple, pairMap, parseInt)

data Move = MUp | MDown | MLeft | MRight

type RopeEnd = (Int, Int)

type Rope = (RopeEnd, RopeEnd) --(head, tail)

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
applyMove (h, t) m =
  let h' = moveHead h m
      t' = moveTail t h'
   in (h', t')
  where
    moveHead :: RopeEnd -> Move -> RopeEnd
    moveHead (hx, hy) MUp = (hx, hy + 1)
    moveHead (hx, hy) MDown = (hx, hy - 1)
    moveHead (hx, hy) MLeft = (hx - 1, hy)
    moveHead (hx, hy) MRight = (hx + 1, hy)

    moveTail :: RopeEnd -> RopeEnd -> RopeEnd
    moveTail (tx, ty) (hx, hy)
      | areTouching (tx, ty) (hx, hy) = (tx, ty)
      | tx == hx = (tx, towards ty hy)
      | ty == hy = (towards tx hx, ty)
      | otherwise = (towards tx hx, towards ty hy)

    areTouching :: RopeEnd -> RopeEnd -> Bool
    areTouching (tx, ty) (hx, hy) = max (abs (hx - tx)) (abs (hy - ty)) < 2

    towards :: Int -> Int -> Int
    towards from to = from + signum (to - from)

ropePositions :: [Move] -> [Rope]
ropePositions moves = positions
  where
    positions = ((0, 0), (0, 0)) : zipWith applyMove positions moves

part1 :: [Move] -> Int
part1 = size . fromList . map snd . ropePositions

solve :: String -> IO (String, String)
solve input = parse input <&> applyTuple (part1, part1) <&> pairMap show