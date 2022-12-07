module Day07 (solve) where

import Data.Functor ((<&>))
import Data.Map as M (Map, adjust, empty, foldl, insert)
import Utils (applyTuple, parseInt)

type Entry = (String, Object)

type Object = FileSystem

data FileSystem = File Int | Directory (Map String Object)

type Path = [String]

data TermLine = CD String | CDOut | CDRoot | LS | Output Entry

parse :: String -> IO FileSystem
parse str = mapM parseLine (lines str) <&> fst . Prelude.foldl applyTermLine (Directory M.empty, [])
  where
    parseLine :: String -> IO TermLine
    parseLine line = case words line of
      ["$", "cd", ".."] -> return CDOut
      ["$", "cd", "/"] -> return CDRoot
      ["$", "cd", dir] -> return $ CD dir
      ["$", "ls"] -> return LS
      ["dir", name] -> return . Output $ (name, Directory M.empty)
      [sizeStr, name] -> do
        fSize <- parseInt sizeStr
        return . Output $ (name, File fSize)
      _ -> fail $ "Could not parse terminal line: " ++ line

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

part1 :: FileSystem -> Int
part1 (File _) = 0
part1 (Directory dir) = M.foldl (\acc obj -> acc + part1 obj) startAcc dir
  where
    size = discSize (Directory dir)
    isSmall = size <= 100000
    startAcc = if isSmall then size else 0

part2 :: Int -> Int -> FileSystem -> Int
part2 capacity required fs = loop fs
  where
    threshold = discSize fs - (capacity - required)
    loop (File _) = maxBound
    loop (Directory dir) = M.foldl (\acc obj -> acc `min` loop obj) startAcc dir
      where
        size = discSize (Directory dir)
        startAcc = if size >= threshold then size else maxBound

solve :: String -> IO (String, String)
solve input =
  parse input
    <&> applyTuple (show . part1, show . part2 70000000 30000000)
