module Day10 (solve) where

import Data.Functor ((<&>))
import Data.List.Split (chunksOf)
import Utils (applyTuple, parseInt)

data Instruction = Noop | Addx Int

type State = Int

parse :: MonadFail m => String -> m [Instruction]
parse = fmap concat . mapM parseLine . lines
  where
    parseLine :: MonadFail m => String -> m [Instruction]
    parseLine line = case words line of
      ["noop"] -> return [Noop]
      ["addx", n] -> do
        n' <- parseInt n
        return [Noop, Addx n'] --for simplicity i assume Addx takes one cycle, adding Noop before
      _ -> fail $ "Could not parse instruction: " ++ line

executeInstruction :: Instruction -> State -> State
executeInstruction Noop = id
executeInstruction (Addx n) = (n +)

simulateProgram :: [Instruction] -> [State]
simulateProgram = scanl (flip executeInstruction) 1

part1 :: [Instruction] -> Int
part1 = sum . map (uncurry (*)) . filter (\(i, _) -> i `mod` 40 == 20) . zip [1 ..] . simulateProgram

part2 :: [Instruction] -> String
part2 ins = '\n' : unlines rows
  where
    sections = take 6 . chunksOf 40 $ simulateProgram ins
    rows = map (zipWith drawSprite [0 ..]) sections

    drawSprite :: Int -> State -> Char
    drawSprite cpuCycle state
      | abs (state - cpuCycle) <= 1 = '#'
      | otherwise = '.'

solve :: MonadFail m => String -> m (String, String)
solve input = parse input <&> applyTuple (show . part1, part2)