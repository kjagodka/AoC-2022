module Day15 (solve) where

import Data.List (nub, partition)
import Data.Maybe (mapMaybe)
import Utils (applyTuple, joinPair, pairMap, parseIntWithTail)

type Coords = (Int, Int)

type Interval = (Int, Int)

type IntervalSet = [Interval]

type Line = (Int, Int) --(a, b) in y = ax + b

parse :: MonadFail m => String -> m [(Coords, Coords)]
parse = mapM parseLine . lines
  where
    parseLine line = case words line of
      ["Sensor", "at", 'x' : '=' : sX, 'y' : '=' : sY, "closest", "beacon", "is", "at", 'x' : '=' : bX, 'y' : '=' : bY] ->
        let sX' = parseIntWithTail "," sX
            sY' = parseIntWithTail ":" sY
            bX' = parseIntWithTail "," bX
            bY' = parseIntWithTail "" bY
         in joinPair (joinPair (sX', sY'), joinPair (bX', bY'))
      _ -> fail $ "Coult not parse line: " ++ line

distance :: Coords -> Coords -> Int
distance (aX, aY) (bX, bY) = abs (bX - aX) + abs (bY - aY)

addIntervalToSet :: IntervalSet -> Interval -> IntervalSet
addIntervalToSet s i =
  let (adjacents, others) = partition (areAdjacent i) s
      merged = foldl intervalSum i adjacents
   in merged : others
  where
    areAdjacent (aFrom, aTo) (bFrom, bTo) = not ((bFrom - aTo > 1) || (aFrom - bTo > 1))
    intervalSum (aFrom, aTo) (bFrom, bTo) = (min aFrom bFrom, max aTo bTo)

intervalSize :: Interval -> Int
intervalSize (from, to) = to - from + 1

intervalSetSize :: IntervalSet -> Int
intervalSetSize s = sum $ map intervalSize s

maxRange :: Int
maxRange = 4000000

hasScannedRow :: Int -> (Coords, Int) -> Bool
hasScannedRow rowY ((_, sY), dist) = abs (sY - rowY) <= dist

scannedInterval :: Int -> (Coords, Int) -> Interval
scannedInterval rowY ((sX, sY), dist) =
  let size = dist - abs (sY - rowY)
   in (sX - size, sX + size)

exludedinRow :: Int -> [(Coords, Coords)] -> IntervalSet
exludedinRow rowY input =
  let scannersDists = map (\(scanner, beacon) -> (scanner, distance scanner beacon)) input
      exludedIntervals = map (scannedInterval rowY) . Prelude.filter (hasScannedRow rowY) $ scannersDists
   in foldl addIntervalToSet [] exludedIntervals

part1 :: [(Coords, Coords)] -> Int
part1 input =
  let beaconsPositions = nub . map snd $ input
      beaconsInRow = length . filter ((== (maxRange `div` 2)) . snd) $ beaconsPositions
   in intervalSetSize (exludedinRow (maxRange `div` 2) input) - beaconsInRow

perimeterLines :: Coords -> Int -> [Line]
perimeterLines (sx, sy) dist =
  let dist' = dist + 1
      upLeft = (-1, sy + sx - dist')
      downLeft = (1, sy - sx + dist')
      upRight = (1, sy - sx - dist')
      downRight = (-1, sy + sx + dist')
   in [upLeft, downLeft, upRight, downRight]

intersection :: Line -> Line -> Maybe Coords
intersection (a1, b1) (a2, b2)
  | a1 == a2 = Nothing
  | odd (b2 - b1) = Nothing
  | otherwise =
    let x = (b2 - b1) `div` (a1 - a2)
        y = a1 * x + b1
     in Just (x, y)

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct as bs = [(a, b) | a <- as, b <- bs]

part2 :: [(Coords, Coords)] -> Int
part2 input =
  let scannersDists = map (\(scanner, beacon) -> (scanner, distance scanner beacon)) input
      lns = concatMap (uncurry perimeterLines) scannersDists
      (goingUp, goingDown) = partition ((== 1) . fst) lns
      intersections = mapMaybe (uncurry intersection) (cartesianProduct goingUp goingDown)
      intersectionsInside = filter isInside intersections
      corners = [(0, 0), (0, maxRange), (maxRange, 0), (maxRange, maxRange)]
      (x, y) = head . nub $ filter notScanned (corners ++ intersectionsInside)
   in x * 4000000 + y
  where
    notScanned candidate = all (\(scanner, beacon) -> distance scanner candidate > distance scanner beacon) input
    isInside (x, y) = (x >= 0) && (x <= maxRange) && (y >= 0) && (y <= maxRange)

solve :: MonadFail m => String -> m (String, String)
solve input = pairMap show . applyTuple (part1, part2) <$> parse input