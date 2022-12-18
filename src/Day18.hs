module Day18 (solve) where

import Data.Set (fromList, notMember)
import Data.List.Split (splitOn)
import Utils (parseInt, pairMap, applyTuple)

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

part1 :: [Coords] -> Int
part1 coords =
  let set = fromList coords
   in sum . map (length . filter (`notMember` set) . faceNeighbours) $ coords

solve :: MonadFail m => String -> m (String, String)
solve input = pairMap show . applyTuple (part1, part1) <$> parse input