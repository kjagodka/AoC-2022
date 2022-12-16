module Utils (showResults, parseInt, parseIntWithTail, pairMap, applyTuple, joinPair, readInputs, splitEvensOdds) where

import Control.Exception (try)

showResults :: (String, String) -> String
showResults (a, b) = "Part1: " ++ a ++ "\nPart2: " ++ b

joinPair :: Monad m => (m a, m b) -> m (a, b)
joinPair (a, b) = do
  a' <- a
  b' <- b
  return (a', b')

pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f (a, b) = (f a, f b)

applyTuple :: (t -> a, t -> b) -> t -> (a, b)
applyTuple (f, g) val = (f val, g val)


parseIntWithTail :: MonadFail m => String -> String -> m Int
parseIntWithTail expected s = case reads s of
  [(i, rest)] ->
    if rest == expected
      then return i
      else fail $ "Could not parse: \"" ++ rest ++ "\" , expected: " ++ expected
  _ -> fail $ "Could not parse integer: " ++ s


parseInt :: MonadFail m => String -> m Int
parseInt s = case reads s of
  [(i, [])] -> return i
  _ -> fail $ "Could not parse integer: " ++ s

readInputs :: Int -> IO String
readInputs n = do
  contentOrExc <- try $ readFile filepath :: IO (Either IOError String)
  case contentOrExc of
    Left _ -> fail $ "Could not read file: " ++ filepath
    Right contents -> return contents
  where
    filepath = "input/Day" ++ show n ++ ".in"

splitEvensOdds :: [a] -> ([a], [a])
splitEvensOdds [] = ([], [])
splitEvensOdds (x:xs) = (x : odds, evens)
  where (evens, odds) = splitEvensOdds xs