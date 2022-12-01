
module Main (main) where
import           System.Environment (getArgs)
import           System.Exit        (exitFailure, exitSuccess)

import           Control.Exception  (try)
import qualified Day01

solvedDays :: [Int]
solvedDays = [1]

solve :: Int -> String -> String
solve 1 = Day01.solve
solve _ = undefined

parse :: [String] -> IO Int
parse [] = putStrLn "Usage: AoC2020 day" >> exitSuccess
parse [day] = case reads day of
                [(d, [])] | d `elem` solvedDays ->  return d
                [(_, [])] ->  putStrLn ("Day " ++ day ++ " is not solved") >> exitSuccess
                _ -> putStrLn ("Could not parse day: " ++ day) >> putStrLn "Expected integer" >> exitFailure
parse _ = putStrLn "Expected just one argument" >> exitFailure


readInputs :: Int -> IO String
readInputs n = do
    contentOrExc <- try $ readFile filepath :: IO (Either IOError String)
    case contentOrExc of
        Left _    -> putStrLn ("Could not read file: " ++ filepath) >> exitFailure
        Right contents -> return contents
    where filepath = "input/Day" ++ show n ++ ".in"

runSolution :: Int -> IO ()
runSolution n = readInputs n >>= putStrLn . solve n


main :: IO ()
main = getArgs >>= parse >>= runSolution

