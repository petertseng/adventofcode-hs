import AdventOfCode (readInputFile)

-- Tried some quick comparisons.
-- Text.Regex.Posix is about 0.029 s
-- Text.Regex.PCRE is about 0.021 s
import Text.Regex.PCRE ((=~))

nice1 :: String -> Bool
nice1 s = not (s =~ "ab|cd|pq|xy") && s =~ "(.)\\1" && s =~ "(.*[aeiou]){3}"

nice2 :: String -> Bool
nice2 s = s =~ "(.).\\1" && s =~ "(..).*\\1"

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  let l = lines s
  print (count nice1 l)
  print (count nice2 l)
