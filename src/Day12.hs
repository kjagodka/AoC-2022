module Day12 (solve) where

import Data.Char (isLower)
import Data.Functor ((<&>))
import Data.Map as Map (Map, empty, filter, findWithDefault, fromList, insert, keys, lookup, member)
import Data.Maybe (mapMaybe)
import Utils (applyTuple, pairMap)

type Coordinates = (Int, Int)

type Distance = Int

data GridSquare = Area Char | Start | End | Outside
  deriving (Eq)

type HeightMap = Map Coordinates GridSquare

type DistanceMap = Map Coordinates Distance

parse :: MonadFail m => String -> m HeightMap
parse str =
  --fmap (fromList . zipWith (\y (x, sq) -> ((x, y), sq)) [0..]) . mapM (fmap (zip [0..]) . mapM parseSquare) . lines
  let squares = mapM (mapM parseSquare) $ lines str
      coords = [zip (repeat x) [0 ..] | x <- [0 ..]]
   in fromList . concat . zipWith zip coords <$> squares
  where
    parseSquare :: MonadFail m => Char -> m GridSquare
    parseSquare 'S' = return Start
    parseSquare 'E' = return End
    parseSquare c
      | isLower c = return $ Area c
      | otherwise = fail $ "Could not parse GridSquare: '" ++ [c] ++ "'"

height :: GridSquare -> Int
height (Area c) = fromEnum c - fromEnum 'a'
height Start = height $ Area 'a'
height End = height $ Area 'z'
height Outside = maxBound

neighbours :: Coordinates -> [Coordinates]
neighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

bfs :: [Coordinates] -> HeightMap -> DistanceMap
bfs startCoords hm = bfsLoop startCoords [] 0 empty
  where
    bfsLoop :: [Coordinates] -> [Coordinates] -> Distance -> DistanceMap -> DistanceMap
    bfsLoop [] [] _ distMap = distMap
    bfsLoop [] next dist distMap = bfsLoop next [] (dist + 1) distMap
    bfsLoop (cur : curs) next dist distMap
      | member cur distMap = bfsLoop curs next dist distMap
      | otherwise = bfsLoop curs (accesible ++ next) dist (insert cur dist distMap)
      where
        accesible = Prelude.filter ((<= (coordHeight cur + 1)) . coordHeight) $ neighbours cur
        coordHeight coord = height $ findWithDefault Outside coord hm

part1 :: HeightMap -> Int
part1 hm =
  let startCoords = keys $ Map.filter (== Start) hm
      dm = bfs startCoords hm
      endCoords = keys $ Map.filter (== End) hm
   in minimum . mapMaybe (`Map.lookup` dm) $ endCoords

part2 :: HeightMap -> Int
part2 hm =
  let startCoords = keys $ Map.filter ((== 0) . height) hm
      dm = bfs startCoords hm
      endCoords = keys $ Map.filter (== End) hm
   in minimum . mapMaybe (`Map.lookup` dm) $ endCoords

solve :: MonadFail m => String -> m (String, String)
solve input = parse input <&> applyTuple (part1, part2) <&> pairMap show