import AdventOfCode (readInputFile)

import Data.List (subsequences)

ways :: [Int] -> [[Int]]
ways weights = filter ((== 150) . sum) (subsequences weights)

main :: IO ()
main = do
  s <- readInputFile
  let weights = map read (lines s)
      validWays = ways weights
      minLength = minimum (map length validWays)
  print (length validWays)
  print (length (filter ((== minLength) . length) validWays))
