module Utils (showResults, parseInt, pairMap, applyTuple) where

import System.Exit (exitFailure)

showResults :: (String, String) -> String
showResults (a, b) = "Part1: " ++ a ++ "\nPart2: " ++ b ++ "\n"

pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f (a, b) = (f a, f b)

applyTuple :: (t -> a, t -> b) -> t -> (a, b)
applyTuple (f, g) val = (f val, g val)

parseInt :: String -> IO Int
parseInt s = case reads s of
  [(i, [])] -> return i
  _ -> putStrLn ("Could not parse integer: " ++ s) >> exitFailure
