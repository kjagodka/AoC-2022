module Day10 (solve) where

import Data.Functor ( (<&>) ) 
import Utils (parseInt, applyTuple, pairMap)

data Instruction = Noop | Addx Int

type State = Int

parse :: String -> IO [Instruction]
parse = fmap concat . mapM parseLine . lines
  where
    parseLine :: String -> IO [Instruction]
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
simulateProgram ins = states
  where states = 1 : zipWith executeInstruction ins states

part1 :: [Instruction] -> Int
part1 = sum . map (uncurry (*)) . filter (\(i, _) -> i `mod` 40 == 20) . zip [1..] . simulateProgram 

part2 :: [Instruction] -> Int
part2 = part1

solve input = parse input <&> applyTuple (part1, part2) <&> pairMap show