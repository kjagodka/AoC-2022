module Day07 (solve) where

import Data.Functor ((<&>))
import Data.Map as M (Map, adjust, empty, foldl, insert)
import Utils (applyTuple, parseInt)

type Entry = (String, Object)

type Object = FileSystem

data FileSystem = File Int | Directory (Map String Object)

type Path = [String]

data TermLine = CD String | CDOut | CDRoot | LS | Output Entry

parse :: MonadFail m => String -> m FileSystem
parse str = mapM parseLine (lines str) <&> fst . Prelude.foldl applyTermLine (Directory M.empty, [])
  where
    parseLine :: MonadFail m => String -> m TermLine
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

directories :: FileSystem -> [Object]
directories (File _) = []
directories (Directory dir) = M.foldl (\acc obj -> acc ++ directories obj) [Directory dir] dir

part1 :: Int -> FileSystem -> Int
part1 threshold = sum . filter (threshold >=) . map discSize . directories

part2 :: Int -> Int -> FileSystem -> Int
part2 capacity required fs = minimum . filter (threshold <=) . map discSize . directories $ fs
  where
    threshold = discSize fs - (capacity - required)

solve :: MonadFail m => String -> m (String, String)
solve input =
  parse input
    <&> applyTuple (show . part1 100000, show . part2 70000000 30000000)
