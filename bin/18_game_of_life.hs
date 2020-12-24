{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)

import qualified Data.Array as Arr
import Data.Array (Array, bounds, listArray)
import Data.Array.Unboxed (UArray, accumArray, assocs)
import Data.List (elemIndices)
import Data.Maybe (mapMaybe)

gameOfLife :: Array Int [Int] -> (Int, Int, Int, Int) -> [Int] -> [Int]
gameOfLife neighs (corner1, corner2, corner3, corner4) = mapMaybe on' . assocs . aliveNeighs neighs
  where on' (pos, _) | pos == corner1 || pos == corner2 || pos == corner3 || pos == corner4 = Just pos
        on' (pos, ns) | 5 <= ns && ns <= 7 = Just pos
        on' (_, _) = Nothing

aliveNeighs :: Array Int [Int] -> [Int] -> UArray Int Int
aliveNeighs neighs = accumArray (+) 0 (bounds neighs) . concatMap oneAndTwos
  where oneAndTwos pos = (pos, 1) : map (, 2) (neighs Arr.! pos)

neigh :: (Int, Int) -> Int -> Int -> [(Int, Int)]
neigh (y, x) height width =
  [(ny, nx) | ny <- yrange, nx <- xrange, (ny, nx) /= (y, x)]
  where (yrange, xrange) = ranges (height - 1) (width - 1) y x

ranges :: Int -> Int -> Int -> Int -> ([Int], [Int])
ranges maxY maxX y x = ([yMin..yMax], [xMin..xMax])
  where yMin = max (y - 1) 0
        yMax = min (y + 1) maxY
        xMin = max (x - 1) 0
        xMax = min (x + 1) maxX

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "uniform of empty"
uniform f (x:xs) = let y = f x in
  if all ((== y) . f) xs then y else error "non-uniform"

main :: IO ()
main = do
  s <- readInputFile
  let rows = lines s
      lights = concat rows
      width = uniform length rows
      height = length rows
      size = width * height
      maxY = height - 1
      maxX = width - 1
      pos (y, x) = y * width + x
      on = elemIndices '#' lights
      neighs = listArray (0, size - 1) [map pos (neigh (y, x) height width) | y <- [0 .. maxY], x <- [0 .. maxX]]
  print (length (iterate (gameOfLife neighs (-1, -1, -1, -1)) on !! 100))
  let potentialCorners = [
          ((0, 0), head (head rows))
        , ((0, maxX), last (head rows))
        , ((maxY, 0), head (last rows))
        , ((maxY, maxX), last (last rows))
        ]
      on' = [pos p | (p, c) <- potentialCorners, c == '.'] ++ on
      [c1, c2, c3, c4] = [pos p | (p, _) <- potentialCorners]
  print (length (iterate (gameOfLife neighs (c1, c2, c3, c4)) on' !! 100))
