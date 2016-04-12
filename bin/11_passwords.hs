import AdventOfCode.Password (nextPassword)

import System.Directory (doesFileExist)
import System.Environment (getArgs)

disambig :: String -> String
disambig "" = ""
disambig ('i':xs) = 'j' : map (const 'a') xs
disambig ('l':xs) = 'm' : map (const 'a') xs
disambig ('o':xs) = 'p' : map (const 'a') xs
disambig (x:xs) = x : disambig xs

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    [] -> readFile "/dev/stdin"
    a:_ -> do
      exist <- doesFileExist a
      if exist then readFile a else return a
  let first = nextPassword (disambig input)
  putStrLn first
  putStrLn (nextPassword first)
