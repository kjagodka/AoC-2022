module Utils ( showResults, parseInt, applyTuple ) where
import           System.Exit (exitFailure)

showResults :: (Int, Int) -> String
showResults (a, b) = "Part1: " ++ show a ++ "\nPart2: " ++ show b ++ "\n"

applyTuple :: (t -> a, t -> b) -> t -> (a, b)
applyTuple (f, g) val = (f val, g val)

parseInt :: String -> IO Int
parseInt s = case reads s of
    [(i, [])] -> return i
    _         -> putStrLn ("Could not parse integer: " ++ s) >> exitFailure
