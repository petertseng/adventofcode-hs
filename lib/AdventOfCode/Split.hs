module AdventOfCode.Split (
  splitOn
, splitOnOne
) where

import Control.Arrow (first)

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitOn c s''
        where (w, s'') = break (== c) s'

splitOnOne :: Char -> String -> (String, String)
splitOnOne _ "" = ("", "")
splitOnOne c (h:t) = if h == c then ("", t)
                     else first (h:) (splitOnOne c t)
