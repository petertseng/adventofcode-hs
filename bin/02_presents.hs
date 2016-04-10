import AdventOfCode (readInputFile)

import Data.List (sort, tails)

paper :: [Int] -> Int
paper dimensions = sum sides * 2 + minimum sides
  where sides = [x * y | x:xs <- tails dimensions, y <- xs]

ribbon :: [Int] -> Int
ribbon dimensions = smallestPerimeter + volume
  where smallestPerimeter = sum (take 2 (sort dimensions)) * 2
        volume = product dimensions

sumBy :: (a -> Int) -> [a] -> Int
sumBy f = sum . map f

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitOn c s''
        where (w, s'') = break (== c) s'

main :: IO ()
main = do
  s <- readInputFile
  let dims = (map (map read . splitOn 'x') . lines) s
  print (sumBy paper dims)
  print (sumBy ribbon dims)
