module Day14 (solve) where

import Control.Monad (zipWithM)
import Data.Set (Set, fromAscList, insert, member, size, unions)
import Utils (applyTuple, joinPair, pairMap, parseInt, splitEvensOdds)

type Coord = Int

type Coords = (Coord, Coord)

type Cave = Set Coords --set of coords filled by rock or sand)

sandSpawner :: Coords
sandSpawner = (500, 0)

parse :: MonadFail m => String -> m (Cave, Coord)
parse str = do
  coordsSeqs <- mapM parseLine . lines $ str
  paths <- unions <$> mapM (\coordsSeq -> unions <$> zipWithM wall coordsSeq (tail coordsSeq)) coordsSeqs
  let abbysY = maximum $ concatMap (map snd) coordsSeqs
   in return (paths, abbysY)
  where
    wall :: MonadFail m => Coords -> Coords -> m (Set Coords)
    wall (x1, y1) (x2, y2)
      | x1 == x2 = return . fromAscList $ [(x1, y) | y <- [(min y1 y2) .. (max y1 y2)]]
      | y1 == y2 = return . fromAscList $ [(x, y1) | x <- [(min x1 x2) .. (max x1 x2)]]
      | otherwise = fail $ "Could not make straight line from: " ++ show (x1, y1) ++ " to: " ++ show (x2, y2)

    parseLine :: MonadFail m => String -> m [Coords]
    parseLine lineStr
      | length arrows == length coordsStrs = fail $ "Expected coords after last arrow in line:\n" ++ lineStr
      | any (/= "->") arrows = fail $ "Could not parse arrow: " ++ head (Prelude.filter (/= "->") arrows)
      | otherwise = mapM parseCoords coordsStrs
      where
        (coordsStrs, arrows) = splitEvensOdds $ words lineStr

    parseCoords :: MonadFail m => String -> m Coords
    parseCoords coordsStr =
      case reads coordsStr of
        [(x, ',' : yStr)] -> joinPair (return x, parseInt yStr)
        _ -> fail $ "Could not parse coords: " ++ coordsStr

dropSand :: Coord -> Cave -> Cave
dropSand abbysY cave = dropSandLoop cave [sandSpawner]
  where
    dropSandLoop filledCave [] = filledCave
    dropSandLoop filledCave trail@((x, y):rest)
      | y >= abbysY = filledCave
      | not $ member (x, y + 1) filledCave = dropSandLoop filledCave ((x, y + 1):trail)
      | not $ member (x - 1, y + 1) filledCave = dropSandLoop filledCave ((x - 1, y + 1):trail)
      | not $ member (x + 1, y + 1) filledCave = dropSandLoop filledCave ((x + 1, y + 1):trail)
      | otherwise = dropSandLoop (insert (x, y) filledCave) rest

dropSand2 :: Coord -> Cave -> Cave
dropSand2 lowestY cave = dropSandLoop cave [sandSpawner]
  where
    dropSandLoop filledCave [] = filledCave
    dropSandLoop filledCave trail@((x, y):rest)
      | y == lowestY + 1 = dropSandLoop (insert (x, y) filledCave) rest
      | not $ member (x, y + 1) filledCave = dropSandLoop filledCave ((x, y + 1):trail)
      | not $ member (x - 1, y + 1) filledCave = dropSandLoop filledCave ((x - 1, y + 1):trail)
      | not $ member (x + 1, y + 1) filledCave = dropSandLoop filledCave ((x + 1, y + 1):trail)
      | otherwise = dropSandLoop (insert (x, y) filledCave) rest

part1 :: (Cave, Coord) -> Int
part1 (cave, abbysY) = size (dropSand abbysY cave) - size cave

part2 :: (Cave, Coord) -> Int
part2 (cave, lowestY) = size (dropSand2 lowestY cave) - size cave

solve :: MonadFail m => String -> m (String, String)
solve input = pairMap show . applyTuple (part1, part2) <$> parse input