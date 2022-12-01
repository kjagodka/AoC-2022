module Utils
    ( combineParts
    ) where

combineParts :: (t -> Int) -> (t -> Int) -> t -> [Char]
combineParts part1 part2 input =
    "Part1: " ++ show (part1 input) ++ "\nPart2: " ++ show (part2 input) ++ "\n"


