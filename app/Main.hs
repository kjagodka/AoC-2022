module Main (main) where
import           Control.Exception  (try)
import           Control.Monad      ((>=>))
import qualified Day01              (solve)
import qualified Day02              (solve)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure, exitSuccess)
import           Utils              (parseInt, showResults)

solvedDays :: [Int]
solvedDays = [1, 2]

solve :: Int -> String -> IO (Int, Int)
solve 1 = Day01.solve
solve 2 = Day02.solve
solve _ = undefined

parseArgs :: [String] -> IO [Int]
parseArgs [] = usage >> exitSuccess
parseArgs ["all"] = return solvedDays
parseArgs s = mapM (parseInt >=> verify) s
    where verify i = if i `elem` solvedDays then return i
                     else putStrLn ("Day: " ++ show i ++ " is not solved") >> exitFailure

usage :: IO ()
usage = do
    putStrLn "Usage: AoC2020 [day]     run solution for days listed in [day]"
    putStrLn "       AoC2022 all       run all implemented solutions"

readInputs :: Int -> IO String
readInputs n = do
    contentOrExc <- try $ readFile filepath :: IO (Either IOError String)
    case contentOrExc of
        Left _    -> putStrLn ("Could not read file: " ++ filepath) >> exitFailure
        Right contents -> return contents
    where filepath = "input/Day" ++ show n ++ ".in"

runSolution :: Int -> IO ()
runSolution n = do
 putStrLn $ "Day " ++ show n
 readInputs n >>= solve n >>= putStrLn . showResults

runSolutions :: [Int] -> IO ()
runSolutions = mapM_ runSolution

main :: IO ()
main = getArgs >>= parseArgs >>= runSolutions

