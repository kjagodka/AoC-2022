module Day11 (solve) where

import Data.List.Split (splitOn)
import Data.Map (Map, adjust, findWithDefault, fromList, insert, keys)
import Text.Read (readMaybe)
import Utils (parseInt)

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

parse :: String -> Monkeys
parse = fromList . map parseMonkey . splitOn [""] . lines
  where
    parseMonkey :: [String] -> (MonkeyId, Monkey)
    parseMonkey [idStr, itemsStr, operationStr, testStr, ifTrueStr, ifFalseStr] =
      let mId = parseId idStr
          mItems = parseItems itemsStr
          mOperation = parseOperation operationStr
          mTest = parseTest (testStr, ifTrueStr, ifFalseStr)
       in (mId, Monkey mItems 0 mOperation mTest)
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

    parseTest :: (String, String, String) -> (Item -> MonkeyId)
    parseTest (testStr, ifTrueStr, ifFalseStr) = case map words [testStr, ifTrueStr, ifFalseStr] of
      [ ["Test:", "divisible", "by", divByStr],
        ["If", "true:", "throw", "to", "monkey", whenTrueStr],
        ["If", "true:", "throw", "to", "monkey", whenFalseStr]
        ] ->
          let divBy = parseInt divByStr
              whenTrue = parseInt whenTrueStr
              whenFalse = parseInt whenFalseStr
           in (\item -> if item `mod` divBy == 0 then whenTrue else whenFalse)
      _ -> error $ "Could not parse test:\n" ++ unlines [testStr, ifTrueStr, ifFalseStr]

catchItem :: Item -> Monkey -> Monkey
catchItem i m = Monkey (items m ++ [i]) (inspected m) (operation m) (test m)

throwItem :: Monkeys -> Item -> MonkeyId -> Monkeys
throwItem monkeys i recipient = adjust (catchItem i) recipient monkeys

execMonkey :: MonkeyId -> Monkeys -> Monkeys
execMonkey monkeyId monkeys =
  if null (items monkey)
    then monkeys
    else
      let item = head $ items monkey
          item' = operation monkey item `div` 3
          recipient = test monkey item'
          monkey' = Monkey (tail $ items monkey) (inspected monkey + 1) (operation monkey) (test monkey)
          monkeys' = insert monkeyId monkey' monkeys
          monkeys'' = throwItem monkeys' item' recipient
       in execMonkey monkeyId monkeys''
  where
    monkey = findWithDefault (Monkey [] 0 id (const 0)) monkeyId monkeys

execRound :: Monkeys -> Monkeys
execRound monkeys = foldl (flip execMonkey) monkeys ids
  where ids = keys monkeys
solve = parse