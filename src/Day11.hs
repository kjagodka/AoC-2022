module Day11 (solve) where

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Map (Map, adjust, elems, findWithDefault, fromList, insert, keys)
import Text.Read (readMaybe)
import Utils (applyTuple, pairMap, parseInt)

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

parse :: MonadFail m => String -> m (Monkeys, Int)
parse input = do
  (monkeyKeys, divisors) <- fmap unzip . mapM parseMonkey . splitOn [""] . lines $ input
  let modulus = foldl lcm 1 divisors
   in return (fromList monkeyKeys, modulus)
  where
    parseMonkey :: MonadFail m => [String] -> m ((MonkeyId, Monkey), Int)
    parseMonkey [idStr, itemsStr, operationStr, testStr, ifTrueStr, ifFalseStr] = do
      mId <- parseId idStr
      mItems <- parseItems itemsStr
      mOperation <- parseOperation operationStr
      (mTest, divBy) <- parseTest (testStr, ifTrueStr, ifFalseStr)
      return ((mId, Monkey mItems 0 mOperation mTest), divBy)
    parseMonkey lns = fail $ "Could not parse monkey:\n" ++ unlines lns

    parseId :: MonadFail m => String -> m MonkeyId
    parseId idStr = case words idStr of
      ["Monkey", monkeyID] ->
        if last monkeyID == ':'
          then parseInt $ init monkeyID
          else fail $ "Could not pars monkeyId: " ++ idStr
      _ -> fail $ "Could not pars monkeyId: " ++ idStr

    parseItems :: MonadFail m => String -> m [Item]
    parseItems itemsStr = case words itemsStr of
      "Starting" : "items:" : ns -> case readMaybe $ "[" ++ unwords ns ++ "]" of
        Just x -> return x
        Nothing -> fail $ "Could not parse monkey items: " ++ itemsStr
      _ -> fail $ "Could not parse monkey items: " ++ itemsStr

    parseOperation :: MonadFail m => String -> m (Item -> Item)
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

    parseOperand :: MonadFail m => String -> m Operand
    parseOperand "old" = return Old
    parseOperand valStr = Val <$> parseInt valStr

    parseOperator :: MonadFail m => String -> m (Item -> Item -> Item)
    parseOperator "+" = return (+)
    parseOperator "*" = return (*)
    parseOperator operatorStr = fail $ "Could not parse operator: " ++ operatorStr

    parseTest :: MonadFail m => (String, String, String) -> m (Item -> MonkeyId, Int)
    parseTest (testStr, ifTrueStr, ifFalseStr) = case map words [testStr, ifTrueStr, ifFalseStr] of
      [ ["Test:", "divisible", "by", divByStr],
        ["If", "true:", "throw", "to", "monkey", whenTrueStr],
        ["If", "false:", "throw", "to", "monkey", whenFalseStr]
        ] -> do
          divBy <- parseInt divByStr
          whenTrue <- parseInt whenTrueStr
          whenFalse <- parseInt whenFalseStr
          return (\item -> if item `mod` divBy == 0 then whenTrue else whenFalse, divBy)
      _ -> fail $ "Could not parse test:\n" ++ unlines [testStr, ifTrueStr, ifFalseStr]

catchItem :: Item -> Monkey -> Monkey
catchItem i m = Monkey (items m ++ [i]) (inspected m) (operation m) (test m)

throwItem :: Monkeys -> Item -> MonkeyId -> Monkeys
throwItem monkeys i recipient = adjust (catchItem i) recipient monkeys

execMonkey :: (Item -> Item) -> MonkeyId -> Monkeys -> Monkeys
execMonkey f monkeyId monkeys =
  if null (items monkey)
    then monkeys
    else
      let item = head $ items monkey
          item' = f $ operation monkey item
          recipient = test monkey item'
          monkey' = Monkey (tail $ items monkey) (inspected monkey + 1) (operation monkey) (test monkey)
          monkeys' = insert monkeyId monkey' monkeys
          monkeys'' = throwItem monkeys' item' recipient
       in execMonkey f monkeyId monkeys''
  where
    monkey = findWithDefault (Monkey [] 0 id (const 0)) monkeyId monkeys

execRound :: (MonkeyId -> Monkeys -> Monkeys) -> Monkeys -> Monkeys
execRound f monkeys = foldl (flip f) monkeys ids
  where
    ids = keys monkeys

getResult :: Monkeys -> Int
getResult = product . take 2 . reverse . sort . map inspected . elems

part1 :: (Monkeys, Int) -> Int
part1 (monkeys, _) = getResult . (!! 20) . iterate (execRound (execMonkey (`div` 3))) $ monkeys

part2 :: (Monkeys, Int) -> Int
part2 (monkeys, modulus) = getResult . (!! 10000) . iterate (execRound (execMonkey (`mod` modulus))) $ monkeys

solve :: MonadFail m => String -> m (String, String)
solve input = pairMap show . applyTuple (part1, part2) <$> parse input