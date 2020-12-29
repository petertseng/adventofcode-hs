import AdventOfCode (readInputFile)

import Data.Containers.ListUtils (nubOrd)
import Data.List (foldl')

-- nubOrd tested to be faster than using a Set,
-- and also faster than using group . sort

type Pos = (Int, Int)

houses :: String -> [Pos]
houses = snd . foldl' moveAndRecord ((0, 0), [(0, 0)])

moveAndRecord :: (Pos, [Pos]) -> Char -> (Pos, [Pos])
moveAndRecord (pos, visits) c = (pos',  pos' : visits)
  where pos' = move pos c

move :: Pos -> Char -> Pos
move (x, y) c = case c of
  '^' -> (x, y + 1)
  'v' -> (x, y - 1)
  '<' -> (x - 1, y)
  '>' -> (x + 1, y)
  '\n' -> (x, y)
  _ -> error (c : ": illegal character")

-- foldr because must preserve order
-- ~ lazy pattern match: https://stackoverflow.com/a/49743871
-- (doesn't really matter for this input, but keep for education)
split :: [a] -> ([a], [a])
split = foldr (\a ~(x, y) -> (a : y, x)) ([], [])

main :: IO ()
main = do
  s <- readInputFile
  let (s1, s2) = split s
      countUnique = length . nubOrd
  print (countUnique (houses s))
  print (countUnique (houses s1 ++ houses s2))
