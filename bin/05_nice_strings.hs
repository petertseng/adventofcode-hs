import AdventOfCode (readInputFile)

import Data.List (isInfixOf, tails)

nice1 :: String -> Bool
nice1 s = count (`elem` "aeiou") s >= 3 && not (any bad pairs) && any (uncurry (==)) pairs
  where bad ('a', 'b') = True
        bad ('c', 'd') = True
        bad ('p', 'q') = True
        bad ('x', 'y') = True
        bad _          = False
        pairs = zip s (drop 1 s)

nice2 :: String -> Bool
nice2 s = aba && any pairRepeats (tails s)
  where aba = any (uncurry (==)) (zip s (drop 2 s))
        pairRepeats (a:b:xs) = [a, b] `isInfixOf` xs
        pairRepeats _ = False

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  let l = lines s
  print (count nice1 l)
  print (count nice2 l)
