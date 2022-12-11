module Day11 (solve) where

import Data.Map ( adjust, Map, fromList)
import Utils (parseInt)
import Text.Read (readMaybe)
import Data.List.Split (splitOn)

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

parse :: String -> IO Monkeys
parse = fmap fromList . mapM parseMonkey . splitOn [""] . lines
  where
    parseMonkey :: [String] -> IO (MonkeyId, Monkey)
    parseMonkey [idStr, itemsStr, operationStr, testStr, ifTrueStr, ifFalseStr] = do
      mId <- parseId idStr
      mItems <- parseItems itemsStr
      mOperation <- parseOperation operationStr
      mTest <- parseTest (testStr, ifTrueStr, ifFalseStr)
      return (mId, Monkey mItems 0 mOperation mTest)
    parseMonkey lns = fail $ "Could not parse monkey:\n" ++ unlines lns

    parseId :: String -> IO MonkeyId
    parseId idStr = case words idStr of
      ["Monkey", id] ->
        if last id == ':'
          then parseInt $ init id
          else fail $ "Could not pars monkeyId: " ++ idStr
      _ -> fail $ "Could not pars monkeyId: " ++ idStr

    parseItems :: String -> IO [Item]
    parseItems itemsStr = case words itemsStr of
      "Starting" : "items:" : ns -> case readMaybe $ "[" ++ unwords ns ++ "]" of
        Just x -> return x
        Nothing -> fail $ "Could not parse monkey items: " ++ itemsStr
      _ -> fail $ "Could not parse monkey items: " ++ itemsStr

    parseOperation :: String -> IO (Item -> Item)
    parseOperation operationStr = case words operationStr of
      ["Operation:", "new", "=", operand1Str, operatorStr, operand2Str] -> do
        operand1 <- parseOperand operand1Str
        operator <- parseOperator operatorStr
        operand2 <- parseOperand operand2Str
        return (\old -> eval operand1 old `operator` eval operand2 old)
      _ -> fail $ "Could not parse operation: " ++ operationStr
      where
        eval :: Operand -> Item -> Item
        eval Old i = i
        eval (Val n) _ = n

    parseOperand :: String -> IO Operand
    parseOperand "old" = return Old
    parseOperand valStr = Val <$> parseInt valStr

    parseOperator :: String -> IO (Item -> Item -> Item)
    parseOperator "+" = return (+)
    parseOperator "*" = return (*)
    parseOperator operatorStr = fail $ "Could not parse operator: " ++ operatorStr

    parseTest :: (String, String, String) -> IO (Item -> MonkeyId) 
    parseTest (testStr, ifTrueStr, ifFalseStr) = case map words [testStr, ifTrueStr, ifFalseStr] of
      [["Test:", "divisible", "by", divByStr],
        ["If", "true:", "throw", "to", "monkey", whenTrueStr],
        ["If", "true:", "throw", "to", "monkey", whenFalseStr]] -> do
        divBy <- parseInt divByStr
        whenTrue <- parseInt whenTrueStr
        whenFalse <- parseInt whenFalseStr
        return (\item -> if item `mod` divBy == 0 then whenTrue else whenFalse)
      _ -> fail $ "Could not parse test:\n" ++ unlines [testStr, ifTrueStr, ifFalseStr]

catchItem :: Item -> Monkey -> Monkey
catchItem i m = Monkey (items m ++ [i]) (inspected m) (operation m) (test m)

throwItem :: Monkeys -> Item -> MonkeyId -> Monkeys
throwItem monkeys i recipient = adjust (catchItem i) recipient monkeys

solve = parse