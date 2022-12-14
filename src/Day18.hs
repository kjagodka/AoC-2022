module Day18 (solve) where

import Data.List.Split (splitOn)
import Data.Set (Set, difference, findMax, fromList, insert, member, notMember, singleton)
import Utils (applyTuple, pairMap, parseInt)

type Coords = (Int, Int, Int)

parse :: MonadFail m => String -> m [Coords]
parse = mapM parseCoords . lines
  where
    parseCoords line = case splitOn [','] line of
      [a, b, c] -> do
        a' <- parseInt a
        b' <- parseInt b
        c' <- parseInt c
        return (a', b', c')
      _ -> fail $ "Could not parse coords: " ++ line

faceNeighbours :: Coords -> [Coords] --list of voxels touching given voxel via face
faceNeighbours (x, y, z) = [(x - 1, y, z), (x + 1, y, z), (x, y - 1, z), (x, y + 1, z), (x, y, z - 1), (x, y, z + 1)]

faceEdgeNeighbours :: Coords -> [Coords] --list of voxels touching given voxel via face or edge
faceEdgeNeighbours (x, y, z) = [(x + dx, y + dy, z + dz) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dz <- [-1, 0, 1], abs dx + abs dy + abs dz `elem` [1, 2]]

part1 :: [Coords] -> Int
part1 coords =
  let set = fromList coords
   in sum . map (length . filter (`notMember` set) . faceNeighbours) $ coords

findConnectedRegion :: Coords -> Set Coords -> (Coords -> [Coords]) -> Set Coords
findConnectedRegion startPoint set neighbours = dfs [startPoint] (singleton startPoint)
  where
    dfs :: [Coords] -> Set Coords -> Set Coords
    dfs [] visited = visited
    dfs (node : stack) visited =
      let notVisitedNeighbours = filter (`notMember` visited) . filter (`member` set) $ neighbours node
          visited' = foldl (flip insert) visited notVisitedNeighbours
       in dfs (notVisitedNeighbours ++ stack) visited'

part2 :: [Coords] -> Int
part2 coords =
  let lava = fromList coords
      air = (`difference` lava) . fromList . concatMap faceEdgeNeighbours $ coords
      outSideair = findConnectedRegion (findMax air) air faceNeighbours
   in sum . map (length . filter (`member` outSideair) . faceNeighbours) $ coords

solve :: MonadFail m => String -> m (String, String)
solve input = pairMap show . applyTuple (part1, part2) <$> parse input