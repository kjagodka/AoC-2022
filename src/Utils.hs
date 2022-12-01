module Utils ( combineParts, parseInt) where
import           System.Exit        (exitFailure)

combineParts :: (t -> Int) -> (t -> Int) -> t -> [Char]
combineParts part1 part2 input =
    "Part1: " ++ show (part1 input) ++ "\nPart2: " ++ show (part2 input) ++ "\n"


parseInt :: String -> IO Int
parseInt s = case reads s of
    [(i, [])] -> return i
    _         -> putStrLn ("Could not parse integer: " ++ s) >> exitFailure
