module Day01 (solve) where
import           Data.Functor    ((<&>))
import           Data.List       (sort)
import           Data.List.Split (splitOn)
import           Utils           (applyTuple, parseInt)

parse :: String -> IO [[Int]]
parse = mapM (mapM parseInt) . splitOn [""] . lines

part1 :: [[Int]] -> Int
part1 = maximum . map sum

part2 :: [[Int]] -> Int
part2 = sum . take 3 . reverse . sort . map sum

solve :: String -> IO (Int, Int)
solve input = parse input <&> applyTuple (part1, part2)
