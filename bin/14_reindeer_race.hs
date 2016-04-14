{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)

import Control.Arrow (second)

runDeer :: (Int, Int, Int) -> [Int]
runDeer = (0 : ) . runDeerFrom 0

runDeerFrom :: Int -> (Int, Int, Int) -> [Int]
runDeerFrom start d@(speed, run, rest) = running ++ resting ++ runDeerFrom end d
  where running = [start + speed, start + speed * 2 .. end]
        resting = replicate rest end
        end = start + speed * run

race :: Int -> [[Int]] -> [(Int, Int)]
race n deer = map (second head) results
  where results = iterate stepRace (map (0,) deer) !! n

stepRace :: [(Int, [Int])] -> [(Int, [Int])]
stepRace deer = map awardLead deer'
  where deer' = map (second tail) deer
        lead = maximum (map (head . snd) deer')
        awardLead (s, l) = (if head l == lead then succ s else s, l)

parseDeer :: String -> (Int, Int, Int)
parseDeer s = case words s of
  [_, "can", "fly", a, "km/s", "for", b, "seconds,", "but", "then", "must", "rest", "for", c, "seconds."] -> (read a, read b, read c)
  _ -> error ("bad deer: " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let deer = map (runDeer . parseDeer) (lines s)
      results = race 2503 deer
  print (maximum (map snd results))
  print (maximum (map fst results))
