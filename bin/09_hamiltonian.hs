import AdventOfCode (readInputFile)

import Data.List (permutations)
import qualified Data.Map.Strict as Map

type Distance = (String, String, Int)
type DistanceMap = Map.Map (String, String) Int

totalDistance :: DistanceMap -> [String] -> Int
totalDistance m path = sum (map (m Map.!) pairs)
  where pairs = zip path (drop 1 path)

distances :: [Distance] -> DistanceMap
distances = Map.fromList . concatMap pairs
  where pairs (a, b, d) = [((a, b), d), ((b, a), d)]

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

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

parseDist :: [String] -> Distance
parseDist [a, "to", b, "=", dist] = (a, b, read dist)
parseDist s = error ("bad distance: " ++ unwords s)

main :: IO ()
main = do
  s <- readInputFile
  let l = map (parseDist . words) (lines s)
      dists = distances l
      paths = permutations (places l)
      totals = map (totalDistance dists) paths
  print (minimum totals)
  print (maximum totals)
