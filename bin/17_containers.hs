import AdventOfCode (readInputFile)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Prelude hiding (rem)

-- This dynamic programming is gratuitous.
-- Brute-forcing through `subsequences` still runs in < 0.1 seconds.
-- However, with dynamic programming we run in about 0.02 seconds.

ways :: [Int] -> Int -> Int -> IntMap Int
ways [] _ _                  = IntMap.empty
ways _ 0 used                = IntMap.singleton used 1
ways _ rem _ | rem < 0       = IntMap.empty
ways [c] rem used | c == rem = IntMap.singleton (used + 1) 1
ways [_] _ _                 = IntMap.empty
ways (c:cs) remaining used   = IntMap.unionWith (+) waysWith waysWithout
  where waysWith = ways cs remaining used
        waysWithout = ways cs (remaining - c) (used + 1)

main :: IO ()
main = do
  s <- readInputFile
  let weights = map read (lines s)
      validWays = ways weights 150 0
  print (sum (IntMap.elems validWays))
  print (snd (IntMap.findMin validWays))
