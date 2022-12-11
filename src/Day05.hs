module Day05 (solve) where

import Data.Char (isLetter)
import Data.List (transpose)
import Data.List.Split (chunksOf, splitOn, wordsBy)
import Data.Map as Map (Map, adjust, elems, findWithDefault, fromAscList, size)
import qualified Data.Map as Map (map)
import Utils (applyTuple, parseInt)

data Crate = Empty | Crate Char
  deriving (Eq, Show)

type Stack = [Crate]

type Cargo = Map Int Stack

type Move = (Int, Int, Int) --(count, from, to)

parse :: String -> (Cargo, [Move])
parse s = case splitOn [""] . lines $ s of
  [cargoPart, movesPart] ->
    let cargo = parseCargo cargoPart
        moves = map (parseMove $ size cargo) movesPart
     in (cargo, moves)
  _ -> error $ "Could not parse input:\n" ++ s
  where
    parseCargo :: [String] -> Cargo
    parseCargo lns = case reverse lns of
      [] -> error "Unable to parsy empty cargo description"
      legend : rows ->
        let legend' = parseLegend legend
            stacks = map (removeEmpty . verifyStack . reverse) . transpose . map (parseRow $ length legend') $ rows
         in fromAscList (zip [1 ..] stacks)

    verifyStack :: Stack -> Stack
    verifyStack stack =
      let countEmpty = length $ filter (== Empty) stack
       in if take countEmpty stack == replicate countEmpty Empty
            then filter (/= Empty) stack
            else error "Stack contains empty spots under crates"

    removeEmpty :: Stack -> Stack
    removeEmpty = filter (/= Empty)

    parseLegend :: String -> [Int]
    parseLegend = map parseInt . wordsBy (== ' ')

    parseRow :: Int -> String -> [Crate]
    parseRow expSize str = verifyRow . map parseCrate . chunksOf 4 $ str ++ " "
      where
        verifyRow crates =
          if length crates == expSize
            then crates
            else error $ "Could not parse row of cargo: " ++ str ++ "\n. Expected size: " ++ show expSize ++ "Actual size: " ++ show (length crates)

    parseCrate :: String -> Crate
    parseCrate "    " = Empty
    parseCrate cr@['[', c, ']', ' '] =
      if isLetter c
        then Crate c
        else error $ "Could not parse crate: " ++ cr
    parseCrate cr = error $ "Could not parse crate: " ++ cr

    parseMove :: Int -> String -> Move
    parseMove expSize mv = case splitOn [' '] mv of
      ["move", a, "from", b, "to", c] ->
        let a' = parseInt a
            b' = parseInt b
            c' = parseInt c
         in if a' > 0 && 1 <= b' && b' <= expSize && 1 <= c' && c' <= expSize
              then (a', b', c')
              else error $ "Encountered integer out of range, when parsing move: " ++ mv
      _ -> error $ "Could not parse move: " ++ mv

makeMove1 :: Cargo -> Move -> Cargo
makeMove1 cargo move@(count, from, to) = dropAt takeFrom
  where
    takeFrom =
      let orig = findWithDefault [] from cargo
       in if (< count) . length $ orig
            then error $ "Not enough crates in stack to make move" ++ show move
            else (adjust (drop count) from cargo, take count orig)
    dropAt (cargo', stack) = adjust (reverse stack ++) to cargo'

makeMove2 :: Cargo -> Move -> Cargo
makeMove2 cargo move@(count, from, to) = dropAt takeFrom
  where
    takeFrom =
      let orig = findWithDefault [] from cargo
       in if (< count) . length $ orig
            then error $ "Not enough crates in stack to make move" ++ show move
            else (adjust (drop count) from cargo, take count orig)
    dropAt (cargo', stack) = adjust (stack ++) to cargo'

crateToChar :: Crate -> Char
crateToChar Empty = error "Tried to show empty"
crateToChar (Crate c) = c

solvePart :: (Cargo -> Move -> Cargo) -> (Cargo, [Move]) -> String
solvePart f (cargo, moves) = map crateToChar . elems . Map.map head $ foldl f cargo moves

part1 :: (Cargo, [Move]) -> String
part1 = solvePart makeMove1

part2 :: (Cargo, [Move]) -> String
part2 = solvePart makeMove2

solve :: String -> (String, String)
solve = applyTuple (part1, part2) . parse
