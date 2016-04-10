import AdventOfCode (readInputFile)

import Control.Monad (foldM)
import Data.List (foldl')
import Prelude hiding (floor)

endFloor :: String -> Int
endFloor = foldl' move 0

firstBasement :: String -> Int
firstBasement = fromLeft . foldM moveM 0 . zip [1 ..]
  where moveM floor (i, c) = let floor' = move floor c in if floor' >= 0 then Right floor' else Left i

move :: Int -> Char -> Int
move floor '(' = floor + 1
move floor ')' = floor - 1
move floor '\n' = floor
move _ c = error (c : ": illegal character")

fromLeft :: Either a b -> a
fromLeft (Left l) = l
fromLeft (Right _) = error "fromLeft of right"

main :: IO ()
main = do
  s <- readInputFile
  print (endFloor s)
  print (firstBasement s)
