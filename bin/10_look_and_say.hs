import Data.Char (digitToInt)
import Data.List (group)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

next :: [Int] -> [Int]
next = concatMap count . group
  where count grp@(elt:_) = [length grp, elt]
        count [] = error "group is broken if it gives an empty list"

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    [] -> readFile "/dev/stdin"
    a:_ -> do
      exist <- doesFileExist a
      if exist then readFile a else return a
  let digits = map digitToInt input
  let i40 = iterate next digits !! 40
  let i50 = iterate next i40 !! 10
  print (length i40)
  print (length i50)
