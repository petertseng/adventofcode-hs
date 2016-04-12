import AdventOfCode (readInputFile)

-- If we could use regex, it would be /\\"|\\\\|\\x..|[^\\]/
-- But we'd have to find non-overlapping matches
literalOverhead :: String -> Int
literalOverhead ('"':s) = 1 + overhead s
  where overhead ('\\':'\\':t)    = 1 + overhead t
        overhead ('\\':'"':t)     = 1 + overhead t
        overhead ('\\':'x':_:_:t) = 3 + overhead t
        overhead ['"']            = 1
        overhead (_:t)            = overhead t
        overhead []               = error ("string does not end with quote: " ++ s)
literalOverhead s = error ("string does not start with quote: " ++ s)

encodedOverhead :: String -> Int
-- 2 for the quotes, then escape any quotes or slashes by prepending a slash
encodedOverhead s = 2 + count '"' s + count '\\' s

sumBy :: (a -> Int) -> [a] -> Int
sumBy f = sum . map f

count :: Char -> String -> Int
count c = length . filter (== c)

main :: IO ()
main = do
  s <- readInputFile
  let l = lines s
  print (sumBy literalOverhead l)
  print (sumBy encodedOverhead l)
