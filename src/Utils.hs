module Utils (showResults, parseInt, pairMap, applyTuple, joinPair, readInputs) where

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