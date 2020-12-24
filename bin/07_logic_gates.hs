import AdventOfCode (readInputFile)

import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)
import Data.Char (isDigit, isLower)
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Word (Word16)

data Input = Name String | Constant Word16

data Gate = Wire Input
          | NotGate Input
          | AndGate Input Input
          | OrGate Input Input
          | LShiftGate Input Input
          | RShiftGate Input Input

values :: Map String Gate -> Map String Word16
values circuit = vals
  where vals = Map.map valueG circuit
        valueG (Wire i)         = valueI i
        valueG (NotGate i)      = complement (valueI i)
        valueG (AndGate a b)    = binOp (.&.) a b
        valueG (OrGate a b)     = binOp (.|.) a b
        valueG (LShiftGate a b) = binOp shiftL16 a b
        valueG (RShiftGate a b) = binOp shiftR16 a b
        valueI (Constant i) = i
        valueI (Name s)     = fromMaybe (error (s ++ " not in circuit")) (Map.lookup s vals)
        binOp op = op `on` valueI

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
      circuit = Map.fromList gates
      a = values circuit Map.! "a"
  print a
  let circuit' = Map.insert "b" (Wire (Constant a)) circuit
  print (values circuit' Map.! "a")
