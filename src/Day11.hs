module Day11 (solve) where

import Data.List.Split (splitOn)
import Data.Map (Map, adjust, findWithDefault, fromList, insert, keys, elems)
import Text.Read (readMaybe)
import Data.List (sort)
import Utils (parseInt, pairMap, applyTuple)

type Item = Int

type MonkeyId = Int

data Monkey = Monkey
  { items :: [Item],
    inspected :: Int,
    operation :: Item -> Item,
    test :: Item -> MonkeyId
  }

data Operand = Old | Val Item

type Monkeys = Map MonkeyId Monkey

parse :: String -> (Monkeys, Int)
parse input = 
  let (monkeyKeys, divisors) = unzip . map parseMonkey . splitOn [""] . lines $ input
      modulus = foldl lcm 1 divisors
   in (fromList monkeyKeys, modulus)
  where
    parseMonkey :: [String] -> ((MonkeyId, Monkey), Int)
    parseMonkey [idStr, itemsStr, operationStr, testStr, ifTrueStr, ifFalseStr] =
      let mId = parseId idStr
          mItems = parseItems itemsStr
          mOperation = parseOperation operationStr
          (mTest, divBy) = parseTest (testStr, ifTrueStr, ifFalseStr)
       in ((mId, Monkey mItems 0 mOperation mTest), divBy)
    parseMonkey lns = error $ "Could not parse monkey:\n" ++ unlines lns

    parseId :: String -> MonkeyId
    parseId idStr = case words idStr of
      ["Monkey", id] ->
        if last id == ':'
          then parseInt $ init id
          else error $ "Could not pars monkeyId: " ++ idStr
      _ -> error $ "Could not pars monkeyId: " ++ idStr

    parseItems :: String -> [Item]
    parseItems itemsStr = case words itemsStr of
      "Starting" : "items:" : ns -> case readMaybe $ "[" ++ unwords ns ++ "]" of
        Just x -> x
        Nothing -> error $ "Could not parse monkey items: " ++ itemsStr
      _ -> error $ "Could not parse monkey items: " ++ itemsStr

    parseOperation :: String -> (Item -> Item)
    parseOperation operationStr = case words operationStr of
      ["Operation:", "new", "=", operand1Str, operatorStr, operand2Str] ->
        let operand1 = parseOperand operand1Str
            operator = parseOperator operatorStr
            operand2 = parseOperand operand2Str
         in (\old -> eval operand1 old `operator` eval operand2 old)
      _ -> error $ "Could not parse operation: " ++ operationStr
      where
        eval :: Operand -> Item -> Item
        eval Old i = i
        eval (Val n) _ = n

    parseOperand :: String -> Operand
    parseOperand "old" = Old
    parseOperand valStr = Val $ parseInt valStr

    parseOperator :: String -> (Item -> Item -> Item)
    parseOperator "+" = (+)
    parseOperator "*" = (*)
    parseOperator operatorStr = error $ "Could not parse operator: " ++ operatorStr

    parseTest :: (String, String, String) -> (Item -> MonkeyId, Int)
    parseTest (testStr, ifTrueStr, ifFalseStr) = case map words [testStr, ifTrueStr, ifFalseStr] of
      [ ["Test:", "divisible", "by", divByStr],
        ["If", "true:", "throw", "to", "monkey", whenTrueStr],
        ["If", "false:", "throw", "to", "monkey", whenFalseStr]
        ] ->
          let divBy = parseInt divByStr
              whenTrue = parseInt whenTrueStr
              whenFalse = parseInt whenFalseStr
           in (\item -> if item `mod` divBy == 0 then whenTrue else whenFalse, divBy)
      _ -> error $ "Could not parse test:\n" ++ unlines [testStr, ifTrueStr, ifFalseStr]

catchItem :: Item -> Monkey -> Monkey
catchItem i m = Monkey (items m ++ [i]) (inspected m) (operation m) (test m)

throwItem :: Monkeys -> Item -> MonkeyId -> Monkeys
throwItem monkeys i recipient = adjust (catchItem i) recipient monkeys

execMonkey1 :: MonkeyId -> Monkeys -> Monkeys
execMonkey1 monkeyId monkeys =
  if null (items monkey)
    then monkeys
    else
      let item = head $ items monkey
          item' = operation monkey item `div` 3
          recipient = test monkey item'
          monkey' = Monkey (tail $ items monkey) (inspected monkey + 1) (operation monkey) (test monkey)
          monkeys' = insert monkeyId monkey' monkeys
          monkeys'' = throwItem monkeys' item' recipient
       in execMonkey1 monkeyId monkeys''
  where
    monkey = findWithDefault (Monkey [] 0 id (const 0)) monkeyId monkeys

execMonkey2 :: Int -> MonkeyId -> Monkeys -> Monkeys
execMonkey2 modulus monkeyId monkeys =
  if null (items monkey)
    then monkeys
    else
      let item = head $ items monkey
          item' = operation monkey item `mod` modulus
          recipient = test monkey item'
          monkey' = Monkey (tail $ items monkey) (inspected monkey + 1) (operation monkey) (test monkey)
          monkeys' = insert monkeyId monkey' monkeys
          monkeys'' = throwItem monkeys' item' recipient
       in execMonkey2 modulus monkeyId monkeys''
  where
    monkey = findWithDefault (Monkey [] 0 id (const 0)) monkeyId monkeys

execRound :: (MonkeyId -> Monkeys -> Monkeys) -> Monkeys -> Monkeys
execRound f monkeys = foldl (flip f) monkeys ids
  where ids = keys monkeys

getResult :: Monkeys -> Int
getResult = product . take 2 . reverse . sort . map inspected . elems

part1 :: (Monkeys, Int) -> Int
part1 (monkeys, _)  = getResult . (!! 20) . iterate (execRound execMonkey1) $ monkeys

part2 :: (Monkeys, Int) -> Int
part2 (monkeys, modulus) = getResult . (!! 10000) . iterate (execRound (execMonkey2 modulus)) $ monkeys

solve :: String -> (String, String)
solve = pairMap show . applyTuple (part1, part2) . parse
