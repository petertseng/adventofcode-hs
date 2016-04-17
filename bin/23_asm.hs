import AdventOfCode (readInputFile)

import Control.Arrow (first, second)
import Control.Monad (forM_)
import Data.Array ((!), Array, bounds)
import Data.Array.IArray (listArray)

data Register = A | B
data Command = Half Register
             | Triple Register
             | Increment Register
             | Jump Int
             | JumpIfEven Register Int
             | JumpIfOne Register Int

run :: Array Int Command -> Int -> (Int, Int) -> (Int, Int)
run commands pc regs | not (pc `between` bounds commands) = regs
run commands pc regs = case commands ! pc of
  Half r         -> op (`div` 2) r
  Triple r       -> op (* 3) r
  Increment r    -> op succ r
  Jump o         -> jump (const True) o
  JumpIfEven r o -> jump (even . readReg r) o
  JumpIfOne  r o -> jump ((== 1) . readReg r) o
  where op f r = run commands (pc + 1) (writeReg r f regs)
        jump p off = run commands (pc + if p regs then off else 1) regs
        writeReg A = first
        writeReg B = second
        readReg A = fst
        readReg B = snd

between :: Int -> (Int, Int) -> Bool
x `between` (i, j) = i <= x && x <= j

parseCommand :: [String] -> Command
parseCommand ["hlf", r]    = Half       (parseRegister r)
parseCommand ["tpl", r]    = Triple     (parseRegister r)
parseCommand ["inc", r]    = Increment  (parseRegister r)
parseCommand ["jmp", o]    = Jump       (parseOffset o)
parseCommand ["jio", r, o] = JumpIfOne  (parseRegister (init r)) (parseOffset o)
parseCommand ["jie", r, o] = JumpIfEven (parseRegister (init r)) (parseOffset o)
parseCommand s             = error ("bad command: " ++ unwords s)

parseRegister :: String -> Register
parseRegister "a" = A
parseRegister "b" = B
parseRegister s   = error ("bad register: " ++ s)

parseOffset :: String -> Int
parseOffset ('+':i) = read i
parseOffset s       = read s

main :: IO ()
main = do
  s <- readInputFile
  let commands = map (parseCommand . words) (lines s)
      arr = listArray (0, length commands - 1) commands
  forM_ [0, 1] (\a -> print (snd (run arr 0 (a, 0))))
