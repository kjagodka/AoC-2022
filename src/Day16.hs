{-# LANGUAGE TupleSections #-}

module Day16 (solve) where

import Data.Map
  ( Map,
    delete,
    findWithDefault,
    fromList,
    insert,
    keys,
    mapKeys,
    notMember,
    singleton,
    unions,
  )
import Utils (applyTuple, joinPair, pairMap, parseIntWithTail)

type Valve = String

type Edge = (Valve, Valve)

type FlowRate = Int

type Distance = Int

parse :: (MonadFail m) => String -> m (Map Valve FlowRate, Map Edge Distance)
parse str = do
  let parsedLines = mapM parseLine $ lines str
      flowRateMap = fromList . Prelude.filter ((> 0) . snd) . Prelude.map (\(valve, flowrate, _) -> (valve, flowrate)) <$> parsedLines
      neighboursMap = fromList . Prelude.map (\(valve, _, neighbours) -> (valve, neighbours)) <$> parsedLines
      distanceMap = generateDistanceMap <$> neighboursMap
   in joinPair (flowRateMap, distanceMap)
  where
    parseLine :: (MonadFail m) => String -> m (Valve, FlowRate, [Valve])
    parseLine line = case words line of
      "Valve" : valve : "has" : "flow" : ('r' : 'a' : 't' : 'e' : '=' : rateStr) : "tunnels" : "lead" : "to" : "valves" : neighbors -> do
        flowrate <- parseIntWithTail ";" rateStr
        return (valve, flowrate, Prelude.map (Prelude.take 2) neighbors)
      ["Valve", valve, "has", "flow", ('r' : 'a' : 't' : 'e' : '=' : rateStr), "tunnel", "leads", "to", "valve", neighbor] -> do
        flowrate <- parseIntWithTail ";" rateStr
        return (valve, flowrate, [neighbor])
      _ -> fail $ "Could not parse line: " ++ line

    generateDistanceMap :: Map Valve [Valve] -> Map Edge Distance
    generateDistanceMap neighboursMap =
      let valves = keys neighboursMap
          distancesFromValves = map (\valve -> bfs [valve] [] 0 (singleton valve 0)) valves
       in unions $ zipWith (\valve distMap -> mapKeys (valve,) distMap) valves distancesFromValves
      where
        bfs :: [Valve] -> [Valve] -> Distance -> Map Valve Distance -> Map Valve Distance
        bfs [] [] _ accum = accum
        bfs [] next distance accum = bfs next [] (distance + 1) accum
        bfs (c : current) next distance accum =
          let neighbours = findWithDefault [] c neighboursMap
              notVisited = filter (`notMember` accum) neighbours
              accum' = foldl (\m neigh -> insert neigh (distance + 1) m) accum notVisited
           in bfs current (notVisited ++ next) distance accum'

explore :: [(Int, Valve)] -> Map Valve FlowRate -> Map Edge Distance -> Int
explore [] _ _ = 0
explore ((time, valve) : others) flowRateMap distMap =
  let choices = keys flowRateMap
      times = map (\choice -> time - 1 - distance choice) choices
      accesible = filter ((> 0) . fst) $ zip times choices
      results =
        if null accesible
          then [0]
          else
            map
              ( \(time', choice) ->
                  let explorers = generateExplorers (time', choice)
                      flowRateMap' = delete choice flowRateMap
                   in time' * flowRate choice + explore explorers flowRateMap' distMap
              )
              accesible
   in maximum results
  where
    distance :: Valve -> Int
    distance to = findWithDefault maxBound (valve, to) distMap

    flowRate :: Valve -> FlowRate
    flowRate v = findWithDefault 0 v flowRateMap

    generateExplorers :: (Int, Valve) -> [(Int, Valve)]
    generateExplorers (time', choice)
      | null others = [(time', choice)]
      | fst (head others) >= time' = [head others, (time', choice)]
      | otherwise = (time', choice) : others

part1 :: (Map Valve FlowRate, Map Edge Distance) -> Int
part1 (flowRateMap, distMap) = explore [(30, "AA")] flowRateMap distMap

part2 :: (Map Valve FlowRate, Map Edge Distance) -> Int
part2 (flowRateMap, distMap) = explore [(26, "AA"), (26, "AA")] flowRateMap distMap

solve :: MonadFail m => String -> m (String, String)
solve input = pairMap show . applyTuple (part1, part2) <$> parse input