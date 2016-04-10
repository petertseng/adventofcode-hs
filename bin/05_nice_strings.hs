import AdventOfCode (readInputFile)

import qualified Data.Map as Map

nice1 :: String -> Bool
nice1 = n1 (0, False, '\0')
  where n1 :: (Int, Bool, Char) -> String -> Bool
        n1 (_, _, 'a') ('b':_) = False
        n1 (_, _, 'c') ('d':_) = False
        n1 (_, _, 'p') ('q':_) = False
        n1 (_, _, 'x') ('y':_) = False
        n1 (vs, pair, prev) (x:xs) = n1 (vs + vowel x, pair || prev == x, x) xs
        n1 (vs, pair, _) [] = pair && vs >= 3
        vowel c = if c `elem` "aeiou" then 1 else 0

nice2 :: String -> Bool
nice2 = n2 ((Map.empty, 0, False), '\0', '\0', False)
  where n2 :: ((Map.Map (Char, Char) Int, Int, Bool), Char, Char, Bool) -> String -> Bool
        n2 ((_, _, True), _, _, True) _ = True
        n2 _ [] = False
        n2 (pairs, a, b, aba) (x:xs) = n2 (add pairs b x, b, x, aba || a == x) xs
        add (m, i, False) a b = case Map.lookup (a, b) m of
          Just j | j + 2 <= i -> (m, i, True)
          Just _ -> (m, i + 1, False)
          Nothing -> (Map.insert (a, b) i m, i + 1, False)
        add m@(_, _, True) _ _ = m

main :: IO ()
main = do
  s <- readInputFile
  let l = lines s
  print (count nice1 l)
  print (count nice2 l)
  where count p = length . filter p
