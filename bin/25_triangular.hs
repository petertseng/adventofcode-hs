import Prelude hiding (exp)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)

seed :: Int
seed = 20151125

base :: Int
base = 252533

modulus :: Int
modulus = 33554393

iterations :: Int -> Int -> Int
iterations row column = (diagonal * diagonal + diagonal) `div` 2 - row
  where diagonal = row + column - 1

modPow :: Int -> Int -> Int -> Int
modPow _ 0 _ = 1
modPow b exp m = modPow' b 1 exp m

modPow' :: Int -> Int -> Int -> Int -> Int
modPow' evens odds exp m | exp < 2 = evens * odds `mod` m
modPow' evens odds exp m = modPow' evens' odds' exp' m
  where evens' = evens *~ evens
        odds'  = if odd exp then odds *~ evens else odds
        exp'   = exp `div` 2
        a *~ b = a * b `mod` m

findNumbers :: String -> [Int]
findNumbers = mapMaybe readMaybe . words

main :: IO ()
main = do
  args <- getArgs
  (row, col) <- case args of
    a:b:_ -> return (read a, read b)
    f:_ -> readFile f >>= (\s -> let [a, b] = findNumbers s in return (a, b))
    _ -> readFile "/dev/stdin" >>= (\s -> let [a, b] = findNumbers s in return (a, b))
  let n = iterations row col
      -- This works because a*b%m == (a%m)*(b%m)%m
      bn = modPow base n modulus
  print (seed * bn `mod` modulus)
