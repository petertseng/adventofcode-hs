import AdventOfCode (readInputFile)

import Control.Monad.State (State, evalState, get, put, runState)
import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)
import Data.Char (isDigit, isLower)
import qualified Data.Map as Map
import qualified Data.Map.Strict as SMap
import Data.Word (Word16)

data Input = Name String | Constant Word16

data Gate = Wire Input
          | NotGate Input
          | AndGate Input Input
          | OrGate Input Input
          | LShiftGate Input Input
          | RShiftGate Input Input

value :: SMap.Map String Gate -> String -> State (Map.Map String Word16) Word16
value circuit name = do
  cache <- get
  case Map.lookup name cache of
    Just v  -> return v
    Nothing -> do
      let comp = case SMap.lookup name circuit of
            Just (Wire i)         -> unOp id i
            Just (NotGate i)      -> unOp complement i
            Just (AndGate a b)    -> binOp (.&.) a b
            Just (OrGate a b)     -> binOp (.|.) a b
            Just (LShiftGate a b) -> binOp shiftL16 a b
            Just (RShiftGate a b) -> binOp shiftR16 a b
            Nothing -> error (name ++ " not in circuit")
      let (v, c) = runState comp cache
      put (Map.insert name v c)
      return v
  where binOp op a b = do
          val1 <- valueI a
          val2 <- valueI b
          return (op val1 val2)
        unOp op a = do
          val <- valueI a
          return (op val)
        valueI (Constant i) = return i
        valueI (Name s)     = value circuit s

shiftL16 :: Word16 -> Word16 -> Word16
shiftL16 a b = shiftL a (fromIntegral b)

shiftR16 :: Word16 -> Word16 -> Word16
shiftR16 a b = shiftR a (fromIntegral b)

parseGate :: [String] -> (String, Gate)
parseGate [a, "->", b]        = (b, Wire (parseInput a))
parseGate ["NOT", a, "->", b] = (b, NotGate (parseInput a))
parseGate [a, op, b, "->", c] = (c, gate (parseInput a) (parseInput b))
  where gate = case op of
          "AND"    -> AndGate
          "OR"     -> OrGate
          "LSHIFT" -> LShiftGate
          "RSHIFT" -> RShiftGate
          _ -> error ("unknown op: " ++ op)
parseGate s = error ("unknown gate: " ++ unwords s)

parseInput :: String -> Input
parseInput "" = error "empty input"
parseInput s | all isDigit s = Constant (read s)
             | all isLower s = Name s
             | otherwise = error ("bad input " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let gates = map (parseGate . words) (lines s)
      circuit = SMap.fromList gates
      a = val circuit "a"
  print a
  let circuit' = Map.insert "b" (Wire (Constant a)) circuit
  print (val circuit' "a")
  where val c s = evalState (value c s) Map.empty
