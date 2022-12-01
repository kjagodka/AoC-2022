module Day01 (solve, main) where
import           Data.List       (sort)
import           Data.List.Split (splitOn)
import           Utils           (combineParts, parseInt)
import Data.Functor ((<&>))

parse :: String -> IO [[Int]]
parse = mapM (mapM parseInt) . splitOn [""] . lines

part1 :: [[Int]] -> Int
part1 = maximum . map sum

part2 :: [[Int]] -> Int
part2 = sum . take 3 . reverse . sort . map sum

solve :: String -> IO String
solve input = parse input <&> combineParts part1 part2

main :: IO ()
main = getContents >>= solve >>= putStrLn