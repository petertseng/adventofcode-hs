module AdventOfCode.Split (
  splitOn
, splitOnOne
) where

import Control.Arrow (second)

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitOn c s''
        where (w, s'') = break (== c) s'

splitOnOne :: Char -> String -> (String, String)
-- drop 1 removes c if it was found.
splitOnOne c = second (drop 1) . break (== c)
