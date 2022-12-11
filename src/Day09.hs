module Day09 (solve) where
  
import Data.Set (fromList, size)
import Utils (applyTuple, pairMap, parseInt)

data Move = MUp | MDown | MLeft | MRight

type Knot = (Int, Int)

type Rope = [Knot]

parse :: String -> [Move]
parse = concatMap parseLine . lines
  where
    parseLine line = case words line of
      [dir, n] ->
        let dir' = parseMove dir
            n' =  parseInt n
         in replicate n' dir'
      _ -> error $ "Could not parse line: " ++ line

    parseMove "U" = MUp
    parseMove "D" = MDown
    parseMove "L" = MLeft
    parseMove "R" = MRight
    parseMove mv = error $ "Could not parse move: " ++ mv

applyMove :: Rope -> Move -> Rope
applyMove (h : t) m = scanl (flip follow) (moveHead h m) t
  where
    moveHead :: Knot -> Move -> Knot
    moveHead (hx, hy) MUp = (hx, hy + 1)
    moveHead (hx, hy) MDown = (hx, hy - 1)
    moveHead (hx, hy) MLeft = (hx - 1, hy)
    moveHead (hx, hy) MRight = (hx + 1, hy)

    follow :: Knot -> Knot -> Knot
    follow (tx, ty) (hx, hy)
      | areTouching (tx, ty) (hx, hy) = (tx, ty)
      | otherwise = (tx + signum (hx - tx), ty + signum (hy - ty))

    areTouching :: Knot -> Knot -> Bool
    areTouching (tx, ty) (hx, hy) = max (abs (hx - tx)) (abs (hy - ty)) < 2
applyMove [] _ = []

simulateRope :: Rope -> [Move] -> [Rope]
simulateRope = scanl applyMove

countTailPositions :: [Rope] -> Int
countTailPositions = size . fromList . map last

part1 :: [Move] -> Int
part1 = countTailPositions . simulateRope (replicate 2 (0, 0))

part2 :: [Move] -> Int
part2 = countTailPositions . simulateRope (replicate 10 (0, 0))

solve :: String -> (String, String)
solve = pairMap show . applyTuple (part1, part2) . parse
