import AdventOfCode (readInputFile)

import Data.Char (isUpper)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- A => BC increases size by 1
-- A => BRnCAr increases size by 3
-- A => BRnCYDAr increases size by 5
-- A => BRnCYDYEAr increases size by 7
-- So to count number of steps to go from e to the final molecule:
-- Count the elements in the final molecule, subtract 1 (we start from e).
-- Subtract 2 for every RnAr pair, subtract 2 for every Y
iterations :: [String] -> Int -> Int
iterations m maxE = length m - count "Rn" * 2 - count "Y" * 2 - (maxE - 1)
  where count p = length (filter (== p) m)

nexts :: Map.Map String [String] -> [String] -> [String]
nexts m s = concatMap (spliceNexts m . flip splitAt s) [0..(length s - 1)]

spliceNexts :: Map.Map String [String] -> ([String], [String]) -> [String]
spliceNexts _ (s, []) = error ("spliceNexts has no after for " ++ unwords s)
spliceNexts m (before, this:after) = map splice choices
  where choices = Map.findWithDefault [] this m
        splice c = concat before ++ c ++ concat after

-- splitBy isUpper "ABCdeFGHiJ" == ["A", "B", "Cde", "F", "G", "Hi", "J"]
-- This could have been achieved by defining:
-- splitBy f = groupBy (\_ b -> (not . f) b)
-- However, I decided this was dangerous.
-- groupBy assumes its equality predicate is an equality.
-- However the provided function (\_ b -> f b)
-- would not be reflexive nor symmertric nor transitive.
-- So it happens to work now, but could be broken in the future,
-- as it breaks the contract of groupBy.
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ []     = []
splitBy f (x:xs) = (x:ys) : splitBy f zs
  where (ys, zs) = break f xs

parseRule :: String -> (String, [String])
parseRule s = case words s of
  [a, "=>", b] -> (a, [b])
  _ -> error ("bad rule: " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let l = lines s
      rulesList = map parseRule (init (init l))
      rules = Map.fromListWith (++) rulesList
      molecule = splitBy isUpper (last l)
      maxE = maximum (map (length . splitBy isUpper) (rules Map.! "e"))
  print (Set.size (Set.fromList (nexts rules molecule)))
  print (iterations molecule maxE)
