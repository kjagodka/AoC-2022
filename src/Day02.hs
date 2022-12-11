module Day02 (solve) where

import Data.List.Split (splitOn)
import Utils (pairMap, applyTuple)

data HandShape = Rock | Paper | Scissors

data GameResult = Win | Draw | Loss
  deriving (Eq)

parseHandShape :: String -> HandShape
parseHandShape "A" = Rock
parseHandShape "B" = Paper
parseHandShape "C" = Scissors
parseHandShape "X" = Rock
parseHandShape "Y" = Paper
parseHandShape "Z" = Scissors
parseHandShape s = error $ "Could not parse handShape: " ++ s

parseGameResult :: String -> GameResult
parseGameResult "X" = Loss
parseGameResult "Y" = Draw
parseGameResult "Z" = Win
parseGameResult s = error $ "Could not parse gameResult: " ++ s

parsePart1 :: String -> [(HandShape, HandShape)]
parsePart1 = map parseLine . lines
  where
    parseLine s =
      case splitOn [' '] s of
        [a, b] ->
          let a' = parseHandShape a
              b' = parseHandShape b
           in (a', b')
        _ -> error $ "Could not parse line: \"" ++ s ++ "\" as pair of handShapes"

parsePart2 :: String -> [(HandShape, GameResult)]
parsePart2 = map parseLine . lines
  where
    parseLine s =
      case splitOn [' '] s of
        [a, b] ->
          let a' = parseHandShape a
              b' = parseGameResult b
           in (a', b')
        _ -> error $ "Could not parse line: \"" ++ s ++ "\" as tuple (handShape, gameResult)"

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

solve :: String -> (String, String)
solve = pairMap show . applyTuple (part1 . parsePart1, part2 . parsePart2)
