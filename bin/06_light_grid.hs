{-# LANGUAGE FlexibleContexts #-}

import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow ((***))
import Control.Monad (forM_)
import Control.Monad.ST (ST)
import Data.Array.Base (UArray)
import Data.Array.IArray (IArray, elems)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.ST (STUArray, runSTUArray)
import Data.Ix (Ix)

type Pos = (Int, Int)

data Command = TurnOn | TurnOff | Toggle

runCommands :: Unboxed a => [(Command, Pos, Pos)] -> (Command -> a -> a) -> Int
runCommands commands language = (reduce . elems) $ runSTUArray $ do
  a <- newSTUArray ((0, 0), (999, 999)) zero
  forM_ commands $ \(command, (x1, y1), (x2, y2)) ->
    forM_ [(x, y) | x <- [x1..x2], y <- [y1..y2]] $ \pos -> do
      val <- readSTUArray a pos
      writeSTUArray a pos (language command val)
  return a

onOrOff :: Command -> Bool -> Bool
onOrOff TurnOn  = const True
onOrOff TurnOff = const False
onOrOff Toggle  = not

brightness :: Command -> Int -> Int
brightness TurnOn  n = n + 1
brightness TurnOff 0 = 0
brightness TurnOff n = n - 1
brightness Toggle  n = n + 2

-- http://stackoverflow.com/questions/2222997/stuarray-with-polymorphic-type
class IArray UArray a => Unboxed a where
  zero :: a
  reduce :: [a] -> Int
  newSTUArray :: Ix i => (i, i) -> a -> ST s (STUArray s i a)
  readSTUArray :: Ix i => STUArray s i a -> i -> ST s a
  writeSTUArray :: Ix i => STUArray s i a -> i -> a -> ST s ()

instance Unboxed Int where
  zero = 0
  reduce = sum
  newSTUArray = newArray
  readSTUArray = readArray
  writeSTUArray = writeArray

instance Unboxed Bool where
  zero = False
  reduce = length . filter id
  newSTUArray = newArray
  readSTUArray = readArray
  writeSTUArray = writeArray

parseCommand :: [String] -> (Command, Pos, Pos)
parseCommand s = (command, start, end)
  where (command, rawCoords) = case s of
          ("turn" : "on" : rest) -> (TurnOn, rest)
          ("turn" : "off" : rest) -> (TurnOff, rest)
          ("toggle" : rest) -> (Toggle, rest)
          _ -> error ("invalid command " ++ unwords s)
        (start, end) = parseCoords rawCoords

parseCoords :: [String] -> (Pos, Pos)
parseCoords s = (parseCoord c1, parseCoord c2)
  where (c1, c2) = case s of
          [a, "through", b] -> (a, b)
          _ -> error ("invalid coordinates " ++ unwords s)

parseCoord :: String -> Pos
parseCoord = (read *** read) . splitOnOne ','

main :: IO ()
main = do
  s <- readInputFile
  let commands = map (parseCommand . words) (lines s)
  print (runCommands commands onOrOff)
  print (runCommands commands brightness)
