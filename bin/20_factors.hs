import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.ST (STUArray)
import Data.List (find)
import Data.Maybe (fromMaybe, fromJust)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

isqrt :: Int -> Int
isqrt = floor' . sqrt . fromIntegral
  -- This is just to avoid a compiler warning
  -- about a defaulted constraint
  where floor' = floor :: (Double -> Int)

gifts :: Maybe Int -> Int -> Int
gifts limit house = sum (map (giftsAt limit house) factors)
  where factors = filter ((== 0) . (house `rem`)) [1..(isqrt house)]

giftsAt :: Maybe Int -> Int -> Int -> Int
giftsAt Nothing house factor1 = factor1 + if factor1 == factor2 then 0 else factor2
  where factor2 = house `div` factor1
giftsAt (Just limit) house factor1 = factor1Val + factor2Val
  where factor2 = house `div` factor1
        factor1Val = if factor2 <= limit then factor1 else 0
        factor2Val = if factor1 <= limit && factor1 /= factor2 then factor2 else 0

smallestGreaterFactorial :: (Int -> Bool) -> (Int, Int)
smallestGreaterFactorial valid = fromJust (find (valid . snd) factorials)
  where factorials = map (\n -> (n, factorial n)) [1..]

factorial :: Int -> Int
factorial n = product [1..n]

houseUpperBound :: (Int -> Bool) -> Int
houseUpperBound valid = foldr (decrease valid) bound [1..n]
  where (n, bound) = smallestGreaterFactorial valid

decrease :: (Int -> Bool) -> Int -> Int -> Int
decrease valid factor bound = fromJust (find valid candidates)
  where boundWithout = bound `div` factor
        candidates = map (* boundWithout) [1..]

-- Euler-Mascheroni constant
gamma :: Double
gamma = 0.57721566490153286060651209008240243104215933593992

-- Robin's inequality
houseLowerBound :: Double -> Int -> Int
houseLowerBound target = binarySearch (valid . fromIntegral) (ceiling guess)
  where valid n = e ** gamma * n * log (log n) >= target
        guess = target / (e ** gamma * log (log target))
        e = exp 1

-- http://stackoverflow.com/questions/24226074/ gives better ways to do this.
-- I am not yet at a place to understand these.
findM :: (Monad m) => [a] -> (a -> m (Maybe b)) -> m (Maybe b)
findM [] _ = return Nothing
findM (x:xs) f = do
  result <- f x
  case result of
    Just _  -> return result
    Nothing -> findM xs f

runElves :: Int -> Int -> Int -> Maybe Int -> Int
runElves target lower upper limit = runST $ do
  arr <- newArray (lower, upper) 0 :: ST s (STUArray s Int Int)

  winner <- findM [1..upper] $ \elf -> do
    let skipped    = if elf < lower then (lower - 1) `div` elf else 0
        startHouse = if elf < lower then (skipped + 1) * elf   else elf
        houses = [startHouse, startHouse + elf .. upper]
        houses' = case limit of
          Just n  -> take (n - skipped) houses
          Nothing -> houses
    forM_ houses' $ \house -> do
      val <- readArray arr house
      writeArray arr house (val + elf)

    if elf >= lower
      then do
        val <- readArray arr elf
        if val >= target
          -- It's safe to terminate now.
          -- No later elf can possibly visit this house.
          then return (Just elf)
          else return Nothing
      else return Nothing

  -- The Nothing case is actually impossible, but that's OK.
  return (fromMaybe upper winner)

binarySearch :: (Int -> Bool) -> Int -> Int -> Int
binarySearch _ lower upper | lower == upper = lower
binarySearch valid lower upper | lower + 1 == upper =
  if valid lower then lower else upper
binarySearch valid lower upper =
  let mid = (lower + upper) `div` 2
      (lower', upper') = if valid mid then (lower, mid) else (mid, upper)
  in binarySearch valid lower' upper'

firstHouse :: Int -> Int -> Maybe Int -> Int
firstHouse target multiplier limit = runElves (ceiling elfValue) lower upper limit
  where upper = houseUpperBound (\n -> gifts limit n * multiplier >= target)
        lower = houseLowerBound elfValue upper
        elfValue = fromIntegral target / fromIntegral multiplier

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    [] -> readFile "/dev/stdin"
    a:_ -> do
      exist <- doesFileExist a
      if exist then readFile a else return a
  let target = read input

  print (firstHouse target 10 Nothing)
  print (firstHouse target 11 (Just 50))
