import Data.List (find)
import Data.Maybe (fromJust)
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

-- Euler-Mascheroni constant
gamma :: Double
gamma = 0.57721566490153286060651209008240243104215933593992

-- Robin's inequality
houseLowerBound :: Double -> Int -> Int
houseLowerBound target = binarySearch (valid . fromIntegral) (ceiling guess)
  where valid n = e ** gamma * n * log (log n) >= target
        guess = target / (e ** gamma * log (log target))
        e = exp 1

binarySearch :: (Int -> Bool) -> Int -> Int -> Int
binarySearch _ lower upper | lower == upper = lower
binarySearch valid lower upper | lower + 1 == upper =
  if valid lower then lower else upper
binarySearch valid lower upper =
  let mid = (lower + upper) `div` 2
      (lower', upper') = if valid mid then (lower, mid) else (mid, upper)
  in binarySearch valid lower' upper'

firstHouse :: Int -> Int -> Maybe Int -> Int
firstHouse target multiplier limit =
  fromJust (find ((>= target) . (* multiplier) . gifts limit) [lower..])
  where lower = houseLowerBound elfValue target
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
