import AdventOfCode (readInputFile)

import Data.List (permutations)
import qualified Data.Map.Strict as Map

type Distance = (String, String, Int)
type DistanceMap = Map.Map (String, String) Int

totalDistance :: DistanceMap -> ([String] -> [(String, String)]) -> [String] -> Int
totalDistance m pairs = sum . map (m Map.!) . pairs

distances :: [Distance] -> DistanceMap
distances = Map.fromListWith (+) . concatMap pairs
  where pairs (a, b, d) = [((a, b), d), ((b, a), d)]

pairsInPath :: [String] -> [(String, String)]
pairsInPath path = zip path (drop 1 path)

pairsInCycle :: [String] -> [(String, String)]
pairsInCycle path = zip path (rotate path)

places :: [Distance] -> [String]
-- Assumption about input ordering:
-- a to b
-- a to c
-- a to d
-- ...
-- b to c
-- given this ordering, just looks at all the a's and extracts their paired location.
places [] = []
places l@((p1, _, _):_) = p1 : rest
  where rest = map snd3 (takeWhile ((p1 ==) . fst3) l)

rotate :: [a] -> [a]
rotate [] = []
rotate (h:t) = t ++ [h]

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

parseDist :: [String] -> Distance
parseDist [a, "would", dir, h, "happiness", "units", "by", "sitting", "next", "to", b] = (a, init b, sign * read h)
  where sign = case dir of
          "gain" -> 1
          "lose" -> -1
          _ -> error ("invalid sign: " ++ dir)
parseDist s = error ("bad distance: " ++ unwords s)

main :: IO ()
main = do
  s <- readInputFile
  let l = map (parseDist . words) (lines s)
      dists = distances l
      paths = permutations (places l)
      -- cycles are invariant to rotation,
      -- so fix the head of the cycle to check fewer of them.
      firstPlace = head (places l)
      cycles = filter ((== firstPlace) . head) paths
      totalsCycle = map (totalDistance dists pairsInCycle) cycles
      totalsPath = map (totalDistance dists pairsInPath) paths
  print (maximum totalsCycle)
  print (maximum totalsPath)
