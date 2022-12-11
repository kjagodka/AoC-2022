import Control.Exception (try)
import Solutions (solve, solvedDays)
import System.Console.ANSI (Color (Red), ColorIntensity (Vivid), ConsoleLayer (Foreground), SGR (Reset, SetColor), setSGR)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO.Error (ioeGetErrorString)
import Utils (parseInt, readInputs, showResults)
import Data.Functor ( (<&>) )

parseArgs :: [String] -> IO [Int]
parseArgs [] = usage >> exitSuccess
parseArgs ["all"] = return solvedDays
parseArgs s = mapM (verify . parseInt) s
  where
    verify i =
      if i `elem` solvedDays
        then return i
        else putStrLn ("Day: " ++ show i ++ " is not solved") >> exitFailure

usage :: IO ()
usage = do
  putStrLn "Usage: AoC2022 [day]     run solution for days listed in [day]"
  putStrLn "       AoC2022 all       run all implemented solutions"

runSolution :: Int -> IO ()
runSolution n = do
  putStrLn $ "Day " ++ show n
  resultsOrError <- try (readInputs n <&> solve n) :: IO (Either IOError (String, String))
  case resultsOrError of
    Right result -> putStrLn $ showResults result
    Left e -> do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ ioeGetErrorString e ++ "\n"
      setSGR [Reset]

runSolutions :: [Int] -> IO ()
runSolutions = mapM_ runSolution

main :: IO ()
main = getArgs >>= parseArgs >>= runSolutions
