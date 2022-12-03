module Day03 (solve) where
import           Data.Char       (isLetter, isLower)
import           Data.Functor    ((<&>))
import           Data.List.Split (chunksOf)
import           Data.Set        (Set, fromList, intersection, toList)
import           Utils           (pairMap)

verify :: String -> IO String
verify s = if all isLetter s && even (length s)
            then return s
            else fail $ "Could not parse line" ++ s

split :: [a] -> ([a], [a])
split xs = splitAt n xs
    where n = length xs `div` 2

parse1 :: String -> IO [(Set Char, Set Char)]
parse1 = mapM parseLine . lines
    where parseLine line = verify line <&> split <&> pairMap fromList

parse2 :: String -> IO [[Set Char]]
parse2 s = fmap (chunksOf 3) (mapM (fmap fromList . verify) . lines $ s)

itemPriority :: Char -> Int
itemPriority c = if isLower c
                    then fromEnum c - fromEnum 'a' + 1
                    else fromEnum c - fromEnum 'A' + 27

part1 :: [(Set Char, Set Char)] -> Int
part1 = sum . map (itemPriority . head . toList . uncurry intersection)

part2 :: [[Set Char]] -> Int
part2 = sum . map (itemPriority . head . toList . foldl1 intersection)

solve :: String -> IO (Int, Int)
solve input = do
    p1 <- parse1 input <&> part1
    p2 <- parse2 input <&> part2
    return (p1, p2)
