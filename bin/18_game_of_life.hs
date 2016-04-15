import AdventOfCode (readInputFile)

import Control.Monad (forM_, when)
import Data.Array.IArray (elems)
import Data.Array.MArray (newListArray, readArray, writeArray)
import Data.Array.ST (runSTUArray)

gameOfLife :: [[Bool]] -> Int -> Bool -> [Bool]
gameOfLife lights n cornersStuck = elems $ runSTUArray $ do
  let len = length lights
  a <- newListArray ((0, 0), (len + 1, len + 1)) (pad lights)
  b <- newListArray ((0, 0), (len + 1, len + 1)) (pad lights)
  when cornersStuck $
    forM_ [(y, x) | y <- [1, len], x <- [1, len]] $ \(y, x) ->
      writeArray b (y, x) True
  forM_ [1..n] $ \i -> do
    let (prev, next) = if even i then (a, b) else (b, a)
    forM_ [(y, x) | y <- [1..len], x <- [1..len]] $ \(y, x) -> do
      let ds = [-1, 0, 1] :: [Int]
          neighbours = [(y+dy, x+dx) | dy <- ds, dx <- ds, (dy, dx) /= (0, 0)]
      neighborVals <- mapM (readArray prev) neighbours
      val <- readArray prev (y, x)
      let newValue = nextState neighborVals val
      writeArray next (y, x) newValue
    when cornersStuck $
      forM_ [(y, x) | y <- [1, len], x <- [1, len]] $ \(y, x) ->
        writeArray next (y, x) True
  -- 100 iterations is even, so we would have written to b.
  return b

nextState :: [Bool] -> Bool -> Bool
nextState neighbours val = case (countTrues neighbours, val) of
  (n, _) | n < 2 -> False
  (2, b)         -> b
  (3, _)         -> True
  _              -> False -- (n > 3)

pad :: [[Bool]] -> [Bool]
pad rows = concat (surround emptyRow (map padRow rows))
  where emptyRow = replicate (length rows + 2) False
        padRow = surround False

surround :: a -> [a] -> [a]
surround x xs = x : xs ++ [x]

countTrues :: [Bool] -> Int
countTrues = length . filter id

toLight :: Char -> Bool
toLight '#' = True
toLight '.' = False
toLight c = error (c : ": invalid character")

main :: IO ()
main = do
  s <- readInputFile
  let rows = map (map toLight) (lines s)
  print (countTrues (gameOfLife rows 100 False))
  print (countTrues (gameOfLife rows 100 True))
