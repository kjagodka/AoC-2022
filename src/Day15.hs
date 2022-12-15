module Day15 (solve) where

import Data.List (sort, partition, nub)
import Utils (applyTuple, joinPair, pairMap)

type Coords = (Int, Int)

type Interval = (Int, Int)

type IntervalSet = [Interval]

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

    parseIntWithTail :: MonadFail m => String -> String -> m Int
    parseIntWithTail expected s = case reads s of
      [(i, rest)] ->
        if rest == expected
          then return i
          else fail $ "Could not parse: \"" ++ rest ++ "\" , expected: " ++ expected
      _ -> fail $ "Could not parse integer: " ++ s

distance :: Coords -> Coords -> Int
distance (aX, aY) (bX, bY) = abs (bX - aX) + abs (bY - aY)

areAdjacent :: Interval -> Interval -> Bool
areAdjacent (aFrom, aTo) (bFrom, bTo) = not ((bFrom - aTo > 1) || (aFrom - bTo > 1))

intervalSum :: Interval -> Interval -> Interval --assumes intervals are adjacent
intervalSum (aFrom, aTo) (bFrom, bTo) = (min aFrom bFrom, max aTo bTo)

intervalProduct :: Interval -> Interval -> Interval
intervalProduct (aFrom, aTo) (bFrom, bTo) = (max aFrom bFrom, min aTo bTo)

addIntervalToSet :: IntervalSet -> Interval -> IntervalSet
addIntervalToSet s i =
  let (adjacents, others) = partition (areAdjacent i) s
      merged = foldl intervalSum i adjacents
   in merged : others 

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
      beaconsInRow = length  . filter ((== (maxRange `div` 2)) . snd) $ beaconsPositions
   in intervalSetSize (exludedinRow (maxRange `div` 2) input) - beaconsInRow

part2 :: [(Coords, Coords)] -> Int
part2 input =
  let rows = [0 .. maxRange]
      searchInterval = (0, maxRange)
      exluded = map (map (intervalProduct searchInterval) . (`exludedinRow` input)) rows
      row = head . filter (\(_, exl) -> intervalSetSize exl < intervalSize searchInterval) $ zip [0 ..] exluded
      y = fst row
      x = case sort . snd $ row of
        [(1, _)] -> 0
        [(0, _)] -> maxRange
        [(0, lower), _] -> lower + 1
        _ -> undefined
   in x * 4000000 + y

solve :: MonadFail m => String -> m (String, String)
solve input = pairMap show . applyTuple (part1, part2) <$> parse input