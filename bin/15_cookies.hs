import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn, splitOnOne)

import Data.List (partition, sortOn)
import Data.Maybe (fromJust, mapMaybe)

type Ingredient = ([Int], Int)

cookie :: [Ingredient] -> (Int, Int, Int) -> Maybe (Int, Int)
cookie ingredients (a, b, c) = cookie' (zipWith scaleBy [a, b, c, d] ingredients)
  where d = 100 - a - b - c

cookie' :: [Ingredient] -> Maybe (Int, Int)
cookie' ingredients = if any (< 0) traits then Nothing
                      else Just (product traits, sum (map calories ingredients))
  where traits = foldr1 (zipWith (+)) (map otherTraits ingredients)

scaleBy :: Int -> Ingredient -> Ingredient
scaleBy n (traits, cals) = (map (* n) traits, cals * n)

limitOfFirst :: [Int] -> Int -> [[Int]] -> Int
-- Start from 1 not 0.
-- My most negative ingredient is the only one with positive durability.
-- So we need at least one of that ingredient.
limitOfFirst base limit (i1:is) = case dropWhile ok [1..limit] of
  []  -> limit -- all numbers were OK.
  h:_ -> h - 1 -- h is first not-OK number, so last OK number must be h - 1.
  where bests = foldr1 (zipWith max) is
        ok = all (> 0) . scores
        scores n = foldr (zipWith (+)) base [map (* n) i1, map (* (limit - n)) bests]
limitOfFirst _ _ [] = error "need at least one ingredient"

otherTraits :: Ingredient -> [Int]
otherTraits = fst

calories :: Ingredient -> Int
calories = snd

-- Assumes that traits come in the same order in all ingredients.
parseIngredient :: String -> Ingredient
parseIngredient s = (otherTraits', calories')
  where calories' = fromJust (lookup "calories" traits)
        otherTraits' = map snd (filter ((/= "calories") . fst) traits)
        (_, rawTraits) = splitOnOne ':' s
        traits = map parseTrait (splitOn ',' rawTraits)

parseTrait :: String -> (String, Int)
parseTrait t = case words t of
  [name, value] -> (name, read value)
  _ -> error ("bad trait: " ++ t)

main :: IO ()
main = do
  s <- readInputFile
  let ingredients = map parseIngredient (lines s)
      sorted = sortOn (minimum . fst) ingredients
      limit = limitOfFirst (repeat 0) 100 (map otherTraits sorted)
      tries = case length ingredients of
        4 -> [(a, b, c) | a <- [0..limit]
                        , b <- [0..limitOfFirst
                                     (map (* a) (otherTraits (head sorted)))
                                     (100 - a)
                                     (map otherTraits (tail sorted))]
                        , c <- [0..(100-a-b)]]
        3 -> [(a, b, 100 - a - b) | a <- [0..100], b <- [0..(100-a)]]
        2 -> [(a, 100 - a, 0) | a <- [0..100]]
        _ -> error "can't handle ingredient list of size other than 2 through 4."
      cookies = mapMaybe (cookie sorted) tries
      (cookies500, cookies') = partition ((== 500) . snd) cookies
      best = maximum . map fst
  print (best cookies')
  print (best cookies500)
