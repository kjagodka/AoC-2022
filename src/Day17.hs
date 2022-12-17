module Day17 (solve) where

import qualified Data.Set as Set
import Utils (applyTuple, pairMap)

type Coords = (Int, Int) -- (height, horizontal for proper ordering)

type Board = Set.Set Coords

type Piece = Set.Set Coords

data Wind = WindLeft | WindRight

parse :: MonadFail m => String -> m [Wind]
parse str = case lines str of
  [line] -> mapM parseWind line
  _ -> fail $ "Could not parse input, expected just one line, got:\n" ++ str
  where
    parseWind '<' = return WindLeft
    parseWind '>' = return WindRight
    parseWind c = fail $ "Could not parse wind '" ++ [c] ++ "'"

tetrisPieces :: [Piece]
tetrisPieces =
  [ Set.fromList [(0, 0), (0, 1), (0, 2), (0, 3)], --horizontal bar
    Set.fromList [(0, 1), (1, 0), (1, 1), (1, 2), (2, 1)], --plus sign shaped
    Set.fromList [(0, 0), (0, 1), (0, 2), (1, 2), (2, 2)], --mirrored L shaped
    Set.fromList [(0, 0), (1, 0), (2, 0), (3, 0)], --vertical bar
    Set.fromList [(0, 0), (0, 1), (1, 0), (1, 1)] --square
  ]

boardWidth :: Int
boardWidth = 7

boardHeight :: Board -> Int
boardHeight board
  | Set.null board = 0
  | otherwise = fst . Set.findMax $ board

dropPieces :: Board -> [Piece] -> [Wind] -> Int -> Board
dropPieces initialBoard allPieces allWinds = dropPiecesLoop initialBoard Set.empty (cycle allPieces) (cycle allWinds)
  where
    dropPiecesLoop :: Board -> Piece -> [Piece] -> [Wind] -> Int -> Board
    dropPiecesLoop board _ _ _ 0 = board
    dropPiecesLoop _ _ [] _ _ = undefined
    dropPiecesLoop _ _ _ [] _ = undefined
    dropPiecesLoop board piece pieces (wind : winds) n
      | null piece = dropPiecesLoop board spawnPiece (tail pieces) (wind : winds) n
      | otherwise =
        let piece' = if isColliding . shiftPiece (0, windAsInt) $ piece then piece else shiftPiece (0, windAsInt) $ piece
            piece'' = shiftPiece (-1, 0) piece'
         in if isColliding piece''
              then dropPiecesLoop (Set.union board piece') Set.empty pieces winds (n - 1)
              else dropPiecesLoop board piece'' pieces winds n
      where
        shiftPiece :: Coords -> Piece -> Piece
        shiftPiece (vecY, vecX) = Set.mapMonotonic (\(pY, pX) -> (pY + vecY, pX + vecX))

        isColliding :: Piece -> Bool
        isColliding p =
          let collidedBorders = not . Set.null . Set.filter (\(y, x) -> y <= 0 || x <= 0 || x > boardWidth) $ p
              collidedDroppedPieces = not $ Set.disjoint p board
           in collidedBorders || collidedDroppedPieces

        spawnPiece :: Piece
        spawnPiece = shiftPiece (boardHeight board + 4, 3) $ head pieces

        windAsInt :: Int
        windAsInt = case wind of
          WindLeft -> -1
          WindRight -> 1

part1 :: [Wind] -> Int
part1 winds = boardHeight $ dropPieces Set.empty tetrisPieces winds 2022

solve :: MonadFail m => String -> m (String, String)
solve input = applyTuple (show . part1, const "not solved") <$> parse input