module Day02 (solve, main) where
import           Data.Functor    ((<&>))
import           Data.List.Split (splitOn)
import           Utils           (applyTuple, showResults)

data HandShape = Rock | Paper | Scissors
data GameResult = Win | Draw | Loss

parseHandShape :: String -> IO HandShape
parseHandShape "A" = return Rock
parseHandShape "B" = return Paper
parseHandShape "C" = return Scissors
parseHandShape "X" = return Rock
parseHandShape "Y" = return Paper
parseHandShape "Z" = return Scissors
parseHandShape c   = fail $ "Could not parse handshape: " ++ c

parse :: String -> IO [(HandShape, HandShape)]
parse = mapM parseLine . filter (/= "") . lines
        where parseLine s =
                case splitOn [' '] s of
                [a, b] -> do
                    a' <- parseHandShape a
                    b' <- parseHandShape b
                    return (a', b')
                _ -> fail $ "Could not parse line: \"" ++ s ++ "\" as pair of handShapes"

playGame :: HandShape -> HandShape -> GameResult
playGame Rock Rock         = Draw
playGame Rock Paper        = Win
playGame Rock Scissors     = Loss
playGame Paper Rock        = Loss
playGame Paper Paper       = Draw
playGame Paper Scissors    = Win
playGame Scissors Rock     = Win
playGame Scissors Paper    = Loss
playGame Scissors Scissors = Draw

gameScore :: GameResult -> Int
gameScore Loss = 0
gameScore Draw = 3
gameScore Win  = 6

shapeScore :: HandShape -> Int
shapeScore Rock     = 1
shapeScore Paper    = 2
shapeScore Scissors = 3


part1 :: [(HandShape, HandShape)] -> Int
part1 = sum . map scoreRound
    where scoreRound (a, b) =
            gameScore (playGame a b) + shapeScore b

part2 :: [(HandShape, HandShape)] -> Int
part2 = sum . map scoreRound
    where scoreRound (a, b) =
            gameScore (playGame a b) + shapeScore b

solve :: String -> IO (Int, Int)
solve input = parse input <&> applyTuple (part1, part2)

main :: IO ()
main = getContents >>= solve >>= putStrLn . showResults
