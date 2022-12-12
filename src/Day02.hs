module Day02 (solve) where

import Data.Functor ((<&>))
import Data.List.Split (splitOn)
import Utils (pairMap)

data HandShape = Rock | Paper | Scissors

data GameResult = Win | Draw | Loss
  deriving (Eq)

parseHandShape :: MonadFail m => String -> m HandShape
parseHandShape "A" = return Rock
parseHandShape "B" = return Paper
parseHandShape "C" = return Scissors
parseHandShape "X" = return Rock
parseHandShape "Y" = return Paper
parseHandShape "Z" = return Scissors
parseHandShape s = fail $ "Could not parse handShape: " ++ s

parseGameResult :: MonadFail m => String -> m GameResult
parseGameResult "X" = return Loss
parseGameResult "Y" = return Draw
parseGameResult "Z" = return Win
parseGameResult s = fail $ "Could not parse gameResult: " ++ s

parsePart1 :: MonadFail m => String -> m [(HandShape, HandShape)]
parsePart1 = mapM parseLine . lines
  where
    parseLine s =
      case splitOn [' '] s of
        [a, b] -> do
          a' <- parseHandShape a
          b' <- parseHandShape b
          return (a', b')
        _ -> fail $ "Could not parse line: \"" ++ s ++ "\" as pair of handShapes"

parsePart2 :: MonadFail m => String -> m [(HandShape, GameResult)]
parsePart2 = mapM parseLine . lines
  where
    parseLine s =
      case splitOn [' '] s of
        [a, b] -> do
          a' <- parseHandShape a
          b' <- parseGameResult b
          return (a', b')
        _ -> fail $ "Could not parse line: \"" ++ s ++ "\" as tuple (handShape, gameResult)"

playGame :: HandShape -> HandShape -> GameResult
playGame Rock Rock = Draw
playGame Rock Paper = Win
playGame Rock Scissors = Loss
playGame Paper Rock = Loss
playGame Paper Paper = Draw
playGame Paper Scissors = Win
playGame Scissors Rock = Win
playGame Scissors Paper = Loss
playGame Scissors Scissors = Draw

gameScore :: GameResult -> Int
gameScore Loss = 0
gameScore Draw = 3
gameScore Win = 6

shapeScore :: HandShape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

scoreRound :: HandShape -> HandShape -> Int
scoreRound a b =
  gameScore (playGame a b) + shapeScore b

part1 :: [(HandShape, HandShape)] -> Int
part1 = sum . map (uncurry scoreRound)

findShape :: HandShape -> GameResult -> HandShape
findShape opponentShape result = head $ filter ((result ==) . playGame opponentShape) [Rock, Paper, Scissors]

part2 :: [(HandShape, GameResult)] -> Int
part2 = sum . map (\(opp, res) -> scoreRound opp $ findShape opp res)

solve :: MonadFail m => String -> m (String, String)
solve input = do
  p1 <- parsePart1 input <&> part1
  p2 <- parsePart2 input <&> part2
  return $ pairMap show (p1, p2)