{-# LANGUAGE FlexibleContexts #-}

import AdventOfCode (readInputFile)

import Control.Monad (foldM_, forM, forM_, when)
import Data.Array.IArray (elems)
import Data.Array.MArray (newListArray, readArray, writeArray)
import Data.Array.ST (runSTUArray)
import Data.Bits (shiftL, testBit)
import Data.Int (Int8)
import Data.Maybe (catMaybes)

gameOfLife :: [[Bool]] -> Int -> Bool -> [Int8]
gameOfLife lights n cornersStuck = elems $ runSTUArray $ do
  let len = length lights
  let maxXY = len - 1
  a <- newListArray ((0, 0), (maxXY, maxXY)) (pack lights)
  when cornersStuck $
    forM_ [(y, x) | y <- [0, maxXY], x <- [0, maxXY]] $ \(y, x) ->
      writeArray a (y, x) 3

  -- Initial setup: Count neighbours.
  forM_ [(y, x) | y <- [0..maxXY], x <- [0..maxXY]] $ \(y, x) -> do
    neighbourVals <- mapM (readArray a) (neighbours maxXY (y, x))
    val <- readArray a (y, x)
    writeArray a (y, x) (val + shiftL (fromIntegral (countAlive neighbourVals)) 2)

  let allCells = [(y, x) | y <- [0..maxXY], x <- [0..maxXY]]

  let step actives _ = do
        changed <- forM actives $ \(y, x) -> do
          val <- readArray a (y, x)
          let isCorner = (x == 0 || x == maxXY) && (y == 0 || y == maxXY)
              isAlive = cornersStuck && isCorner || 10 <= val && val <= 15
          if isAlive /= alive val
            then return (Just (y, x))
            else do
              writeArray a (y, x) (val - 1)
              return Nothing

        let changed' = catMaybes changed

        newActives <- forM changed' $ \(y, x) -> do
          val <- readArray a (y, x)
          let delta = if alive val then -4 else 4
          newThisCell <- forM (neighboursAndSelf maxXY (y, x)) $ \(ny, nx) -> do
            nval <- readArray a (ny, nx)
            if even nval
              then do
                writeArray a (ny, nx) (nval + delta + 1)
                return (Just (ny, nx))
              else do
                writeArray a (ny, nx) (nval + delta)
                return Nothing

          val' <- readArray a (y, x)
          writeArray a (y, x) (val' - (delta `div` 2))
          return (catMaybes newThisCell)

        return (changed' ++ concat newActives)

  foldM_ step allCells [1..n]
  return a

neighbours :: Int -> (Int, Int) -> [(Int, Int)]
neighbours maxXY (y, x) =
  [(ny, nx) | ny <- yrange, nx <- xrange, (ny, nx) /= (y, x)]
  where (yrange, xrange) = ranges maxXY y x

-- The duplication is regrettable, but does save some time.
neighboursAndSelf :: Int -> (Int, Int) -> [(Int, Int)]
neighboursAndSelf maxXY (y, x) = [(ny, nx) | ny <- yrange, nx <- xrange]
  where (yrange, xrange) = ranges maxXY y x

ranges :: Int -> Int -> Int -> ([Int], [Int])
ranges maxXY y x = ([yMin..yMax], [xMin..xMax])
  where yMin = max (y - 1) 0
        yMax = min (y + 1) maxXY
        xMin = max (x - 1) 0
        xMax = min (x + 1) maxXY

-- Packs lights into their byte form:
-- bit 0 is active/inactive
-- bit 1 is alive/dead
-- bits 2+ are neighbour count
pack :: [[Bool]] -> [Int8]
pack = concatMap (map (\b -> if b then 3 else 1))

alive :: Int8 -> Bool
alive = flip testBit 1

countAlive :: [Int8] -> Int
countAlive = length . filter alive

toLight :: Char -> Bool
toLight '#' = True
toLight '.' = False
toLight c = error (c : ": invalid character")

main :: IO ()
main = do
  s <- readInputFile
  let rows = map (map toLight) (lines s)
  print (countAlive (gameOfLife rows 100 False))
  print (countAlive (gameOfLife rows 100 True))
