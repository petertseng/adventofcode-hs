import AdventOfCode (readInputFile)

import qualified Data.Map as Map

nice1 :: String -> Bool
nice1 s = count (`elem` "aeiou") s >= 3 && not (any bad pairs) && any (uncurry (==)) pairs
  where bad ('a', 'b') = True
        bad ('c', 'd') = True
        bad ('p', 'q') = True
        bad ('x', 'y') = True
        bad _          = False
        pairs = zip s (drop 1 s)

nice2 :: String -> Bool
nice2 s = aba && repeatingPair Map.empty (zip3 s (drop 1 s) [0, 1 ..])
  where aba = any (uncurry (==)) (zip s (drop 2 s))
        repeatingPair :: Map.Map (Char, Char) Int -> [(Char, Char, Int)] -> Bool
        repeatingPair _ [] = False
        repeatingPair pairs ((a, b, i):xs) = case Map.lookup (a, b) pairs of
          Just j | j + 2 <= i -> True
          Just _ -> repeatingPair pairs xs
          Nothing -> repeatingPair (Map.insert (a, b) i pairs) xs

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  let l = lines s
  print (count nice1 l)
  print (count nice2 l)
