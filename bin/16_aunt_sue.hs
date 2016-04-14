import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn, splitOnOne)

import qualified Data.Map as Map
import Data.Maybe (fromJust)

type Aunt = (String, [Trait])
type Trait = (String, Int)

desires :: Map.Map String (Int, Int -> Int -> Bool)
desires = Map.fromList [
            ("children", (3, (==)))
          , ("cats", (7, (>)))
          , ("samoyeds", (2, (==)))
          , ("pomeranians", (3, (<)))
          , ("akitas", (0, (==)))
          , ("vizslas", (0, (==)))
          , ("goldfish", (5, (<)))
          , ("trees", (3, (>)))
          , ("cars", (2, (==)))
          , ("perfumes", (1, (==)))
          ]

type ConstraintTransform = (Int -> Int -> Bool) -> Int -> Int -> Bool

goodAunt :: ConstraintTransform -> Aunt -> Bool
goodAunt transform (_, traits) = all (satisfy transform) traits

satisfy :: ConstraintTransform -> Trait -> Bool
satisfy transform (name, value) = transform f value value'
  where (value', f) = fromJust (Map.lookup name desires)

parseAunt :: String -> Aunt
parseAunt s = (name, traits)
  where (name, rawTraits) = splitOnOne ':' s
        traits = map parseTrait (splitOn ',' rawTraits)

parseTrait :: String -> Trait
parseTrait s = (tail name, read value)
  where (name, value) = splitOnOne ':' s

main :: IO ()
main = do
  s <- readInputFile
  let aunts = map parseAunt (lines s)
      aunt1 = filter (goodAunt (const (==))) aunts
      aunt2 = filter (goodAunt id) aunts
  mapM_ (putStrLn . last . words . fst) aunt1
  mapM_ (putStrLn . last . words . fst) aunt2
