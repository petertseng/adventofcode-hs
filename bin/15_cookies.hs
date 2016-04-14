import AdventOfCode (readInputFile)

import Data.List (sortOn)
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
limitOfFirst base limit (i1:is) = case takeWhile ok [1..limit] of
  [] -> 0 -- Even 1 isn't OK?!
  l -> last l
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
  where calories' = fromJust (lookup "calories" traitsAndNames)
        otherTraits' = map snd (filter ((/= "calories") . fst) traitsAndNames)
        traitsAndNames = case splitOn ':' s of
          [_, traits] -> map parseTrait (splitOn ',' traits)
          _ -> error ("bad cookie: " ++ s)

parseTrait :: String -> (String, Int)
parseTrait t = case words t of
  [name, value] -> (name, read value)
  _ -> error ("bad trait: " ++ t)

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitOn c s''
        where (w, s'') = break (== c) s'

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
      best = maximum . map fst
  print (best cookies)
  print (best (filter ((== 500) . snd) cookies))
