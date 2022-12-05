module Day05 (solve) where

import Control.Monad (foldM)
import Data.Char (isLetter)
import Data.Functor ((<&>))
import Data.List (transpose)
import Data.List.Split (chunksOf, splitOn, wordsBy)
import Data.Map as Map (Map, adjust, findWithDefault, foldr, fromAscList, size)
import Utils (applyTuple, joinPair, parseInt)

data Crate = Empty | Crate Char
  deriving (Eq, Show)

type Stack = [Crate]

type Cargo = Map Int Stack

type Move = (Int, Int, Int) --(count, from, to)

parse :: String -> IO (Cargo, [Move])
parse s = case splitOn [""] . lines $ s of
  [cargoPart, movesPart] -> do
    cargo <- parseCargo cargoPart
    moves <- mapM (parseMove $ size cargo) movesPart
    return (cargo, moves)
  _ -> fail $ "Could not parse input:\n" ++ s
  where
    parseCargo :: [String] -> IO Cargo
    parseCargo lns = case reverse lns of
      [] -> fail "Unable to parsy empty cargo description"
      legend : rows -> do
        legend' <- parseLegend legend
        stacks <- mapM (parseRow $ length legend') rows >>= mapM (verifyStack . reverse) . transpose <&> map removeEmpty
        return $ fromAscList (zip [1 ..] stacks)

    verifyStack :: Stack -> IO Stack
    verifyStack stack =
      let countEmpty = length $ filter (== Empty) stack
       in if take countEmpty stack == replicate countEmpty Empty
            then return $ filter (/= Empty) stack
            else fail "Stack contains empty spots under crates"

    removeEmpty :: Stack -> Stack
    removeEmpty = filter (/= Empty)

    parseLegend :: String -> IO [Int]
    parseLegend str = mapM parseInt (wordsBy (== ' ') str)

    parseRow :: Int -> String -> IO [Crate]
    parseRow expSize str = (mapM parseCrate . chunksOf 4 $ str ++ " ") >>= verifyRow
      where
        verifyRow crates =
          if length crates == expSize
            then return crates
            else fail $ "Could not parse row of cargo: " ++ str ++ "\n. Expected size: " ++ show expSize ++ "Actual size: " ++ show (length crates)

    parseCrate :: String -> IO Crate
    parseCrate "    " = return Empty
    parseCrate cr@['[', c, ']', ' '] =
      if isLetter c
        then return $ Crate c
        else fail $ "Could not parse crate: " ++ cr
    parseCrate cr = fail $ "Could not parse crate: " ++ cr

    parseMove :: Int -> String -> IO Move
    parseMove expSize mv = case splitOn [' '] mv of
      ["move", a, "from", b, "to", c] -> do
        a' <- parseInt a
        b' <- parseInt b
        c' <- parseInt c
        if a' > 0 && 1 <= b' && b' <= expSize && 1 <= c' && c' <= expSize
          then return (a', b', c')
          else fail $ "Encountered integer out of range, when parsing move: " ++ mv
      _ -> fail $ "Could not parse move: " ++ mv

makeMove1 :: (MonadFail m) => Cargo -> Move -> m Cargo
makeMove1 cargo move@(count, from, to) = takeFrom <&> dropAt
  where
    takeFrom =
      let orig = findWithDefault [] from cargo
       in if (< count) . length $ orig
            then fail $ "Not enough crates in stack to make move" ++ show move
            else return (adjust (drop count) from cargo, take count orig)
    dropAt (cargo', stack) = adjust (reverse stack ++) to cargo'

makeMove2 :: (MonadFail m) => Cargo -> Move -> m Cargo
makeMove2 cargo move@(count, from, to) = takeFrom <&> dropAt
  where
    takeFrom =
      let orig = findWithDefault [] from cargo
       in if (< count) . length $ orig
            then fail $ "Not enough crates in stack to make move" ++ show move
            else return (adjust (drop count) from cargo, take count orig)
    dropAt (cargo', stack) = adjust (stack ++) to cargo'

crateToChar :: MonadFail m => Crate -> m Char
crateToChar Empty = fail "Tried to show empty"
crateToChar (Crate c) = return c

solvePart :: (MonadFail m) => (Cargo -> Move -> m Cargo) -> (Cargo, [Move]) -> m String
solvePart f (cargo, moves) =
  foldM f cargo moves
    >>= mapM crateToChar . Map.foldr (\stack acc -> head stack : acc) []

part1 :: (Cargo, [Move]) -> IO String
part1 = solvePart makeMove1

part2 :: (Cargo, [Move]) -> IO String
part2 = solvePart makeMove2

solve :: String -> IO (String, String)
solve input = parse input >>= joinPair . applyTuple (part1, part2)
