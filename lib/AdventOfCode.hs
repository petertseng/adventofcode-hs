module AdventOfCode (
  readInputFile
) where

import System.Environment (getArgs)

readInputFile :: IO String
readInputFile = do
  args <- getArgs
  let f = case args of
        [] -> "/dev/stdin"
        a:_ -> a
  readFile f
