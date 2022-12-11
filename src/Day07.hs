module Day07 (solve) where

import Data.Map(Map, adjust, empty, insert)
import qualified Data.Map as M (foldl)
import Utils (applyTuple, parseInt, pairMap)

type Entry = (String, Object)

type Object = FileSystem

data FileSystem = File Int | Directory (Map String Object)

type Path = [String]

data TermLine = CD String | CDOut | CDRoot | LS | Output Entry

parse :: String -> FileSystem
parse = fst . foldl applyTermLine (Directory empty, []) . map parseLine . lines
  where
    parseLine :: String -> TermLine
    parseLine line = case words line of
      ["$", "cd", ".."] -> CDOut
      ["$", "cd", "/"] -> CDRoot
      ["$", "cd", dir] -> CD dir
      ["$", "ls"] -> LS
      ["dir", name] -> Output (name, Directory empty)
      [sizeStr, name] -> do
        let fSize = parseInt sizeStr
         in Output (name, File fSize)
      _ -> error $ "Could not parse terminal line: " ++ line

    applyTermLine :: (FileSystem, Path) -> TermLine -> (FileSystem, Path)
    applyTermLine (fs, path) (CD dir) = (fs, path ++ [dir])
    applyTermLine (fs, path) CDOut = (fs, take (length path - 1) path)
    applyTermLine (fs, _) CDRoot = (fs, [])
    applyTermLine (fs, path) LS = (fs, path)
    applyTermLine (fs, path) (Output entry) = (addEntry path entry fs, path)

    addEntry :: Path -> Entry -> FileSystem -> FileSystem
    addEntry (dir : path') entry (Directory fs) = Directory $ adjust (addEntry path' entry) dir fs
    addEntry [] (name, object) (Directory fs) = Directory $ insert name object fs
    addEntry _ _ (File _) = undefined

discSize :: Object -> Int
discSize (File n) = n
discSize (Directory dir) = M.foldl (\acc obj -> acc + discSize obj) 0 dir

directories :: FileSystem -> [Object]
directories (File _) = []
directories (Directory dir) = M.foldl (\acc obj -> acc ++ directories obj) [Directory dir] dir

part1 :: Int -> FileSystem -> Int
part1 threshold = sum . filter (threshold >=) . map discSize . directories

part2 :: Int -> Int -> FileSystem -> Int
part2 capacity required fs = minimum . filter (threshold <=) . map discSize . directories $ fs
  where
    threshold = discSize fs - (capacity - required)

solve :: String -> (String, String)
solve = pairMap show . applyTuple (part1 100000, part2 70000000 30000000) . parse
