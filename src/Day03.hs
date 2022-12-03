module Day03 (solve) where
import           Data.Char    (isLetter, isLower)
import           Data.Functor ((<&>))
import           Data.Set     (Set, fromList, toList, intersection)
import           Utils        (applyTuple, pairMap)

verify :: String -> IO String
verify s = if all isLetter s && even (length s)
            then return s
            else fail $ "Could not parse line" ++ s

split :: [a] -> ([a], [a])
split xs = splitAt n xs
    where n = length xs `div` 2

parse :: String -> IO [(Set Char, Set Char)]
parse = mapM parseLine . lines
    where parseLine line = verify line <&> split <&> pairMap fromList

itemPriority :: Char -> Int
itemPriority c = if isLower c
                    then fromEnum c - fromEnum 'a' + 1
                    else fromEnum c - fromEnum 'A' + 27

part1 :: [(Set Char, Set Char)] -> Int
part1 = sum . map (itemPriority . head . toList . uncurry intersection)

part2 :: [(Set Char, Set Char)] -> Int
part2 = part1

solve :: String -> IO (Int, Int)
solve input = parse input <&> applyTuple (part1, part2)
