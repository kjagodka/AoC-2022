module Day05 (solve) where

import Control.Monad (foldM)
import Data.Char (isLetter)
import Data.Functor ((<&>))
import Data.List.Split (chunksOf, splitOn)
import Data.Map as Map (Map, adjust, alter, empty, findWithDefault, foldr, insertWith)
import Data.Maybe (fromMaybe)
import Utils (applyTuple, joinPair, parseInt)

data Crate = Empty | Crate Char
  deriving (Eq)

type Stack = [Crate]

type Cargo = Map Int Stack

type Move = (Int, Int, Int) --(count, from, to)

parse :: String -> IO (Cargo, [Move])
parse s = case splitOn [""] . lines $ s of
  [cargoPart, movesPart] -> joinPair (parseCargo cargoPart, mapM parseMove movesPart)
  _ -> fail $ "Could not parse input:\n" ++ s
  where
    parseCargo :: [String] -> IO Cargo
    parseCargo lns = do
      rows <- mapM parseRow . tail $ reverse lns
      return $ foldl addRow empty rows

    parseRow :: String -> IO [Crate]
    parseRow str = mapM parseCrate . chunksOf 4 $ str ++ " "

    addRow :: Cargo -> [Crate] -> Cargo
    addRow cargo row =
      let numbered = filter ((/= Empty) . snd) . zip [1 ..] $ row
          addCrate cr = alter (Just . (cr :) . fromMaybe [])
       in foldl (\cargo' (key, crate) -> addCrate crate key cargo') cargo numbered

    parseCrate :: String -> IO Crate
    parseCrate "    " = return Empty
    parseCrate cr@['[', c, ']', ' '] =
      if isLetter c
        then return $ Crate c
        else fail $ "Could not parse crate: " ++ cr
    parseCrate cr = fail $ "Could not parse crate: " ++ cr

    parseMove :: String -> IO Move
    parseMove mv = case splitOn [' '] mv of
      ["move", a, "from", b, "to", c] -> do
        a' <- parseInt a
        b' <- parseInt b
        c' <- parseInt c
        if all (>= 0) [a', b', c']
          then return (a', b', c')
          else fail $ "Encountered negative integer, when parsing move: " ++ mv
      _ -> fail $ "Could not parse move: " ++ mv

makeMove1 :: (MonadFail m) => Cargo -> Move -> m Cargo
makeMove1 cargo move@(count, from, to) = takeFrom >>= dropAt
  where
    takeFrom =
      let orig = findWithDefault [] from cargo
       in if (< count) . length $ orig
            then fail $ "Not enough crates in stack to make move" ++ show move
            else return (adjust (drop count) from cargo, take count orig)
    dropAt (cargo', stack) = return $ insertWith (++) to (reverse stack) cargo'

makeMove2 :: (MonadFail m) => Cargo -> Move -> m Cargo
makeMove2 cargo move@(count, from, to) = takeFrom >>= dropAt
  where
    takeFrom =
      let orig = findWithDefault [] from cargo
       in if (< count) . length $ orig
            then fail $ "Not enough crates in stack to make move" ++ show move
            else return (adjust (drop count) from cargo, take count orig)
    dropAt (cargo', stack) = return $ insertWith (++) to stack cargo'

crateToChar :: MonadFail m => Crate -> m Char
crateToChar Empty = fail "Tried to show empty"
crateToChar (Crate c) = return c


solvePart :: (MonadFail m) => (Cargo -> Move -> m Cargo) -> (Cargo, [Move]) -> m String
solvePart f (cargo, moves) =
  foldM f cargo moves
    <&> Map.foldr (\stack acc -> head stack : acc) []
    >>= mapM crateToChar


part1 :: (Cargo, [Move]) -> IO String
part1 = solvePart makeMove1

part2 :: (Cargo, [Move]) -> IO String
part2 = solvePart makeMove2

solve :: String -> IO (String, String)
solve input = parse input <&> applyTuple (part1, part2) >>= joinPair
