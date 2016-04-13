import AdventOfCode (readInputFile)

import Control.Monad (foldM)
import Data.Char (isDigit)
import Text.Read (readMaybe)

data JsonNest = Object Bool | Array deriving (Show)

-- Left are for possible but malformed input strings, like } or ].
-- error are for invalid states that no input string, well-formed or malformed, should ever cause.

sums :: String -> Either String (Int, Int)
sums s = foldM parse empty s >>= expectOneSum
  where expectOneSum r = case objSum r of
          [a] -> Right (total r, a)
          [] -> error ("should always have 1+ objSums: " ++ show r)
          _ -> Left ("too many sums (unclosed object): " ++ show r)

data ParseState = ParseState {
  objSum :: [Int]
, total :: !Int
, nest :: [JsonNest]
, inString :: Bool
, numBuf :: String
, redStart :: Int
, redGood :: Int
, idx :: Int
} deriving Show

empty :: ParseState
empty = ParseState {
  objSum = [0]
, total = 0
, nest = []
, inString = False
, numBuf = ""
, redStart = 0
, redGood = 0
, idx = 0
}

pushNest :: JsonNest -> ParseState -> ParseState
pushNest state ps = ps { nest = state : nest ps }

parseNum :: ParseState -> Either String ParseState
parseNum ps@ParseState { numBuf = "" } = Right ps
parseNum ps@ParseState { objSum = [] } = error ("should always have 1+ objSums: " ++ show ps)
parseNum ps@ParseState { total = t, objSum = o:os, numBuf = bf } = case readMaybe (reverse bf) of
  Just i  -> Right (ps { total = t + i, objSum = (o + i):os, numBuf = "" })
  Nothing -> Left ("not a number: " ++ bf)

closeObj :: ParseState -> Either String ParseState
closeObj ps@ParseState { objSum = a:b:os, nest = Object red:ns } =
  Right (ps { objSum = (b + if red then 0 else a):os, nest = ns })
closeObj ps@ParseState { nest = Object _:_ } = error ("should always have 2+ objSums when closing obj: " ++ show ps)
closeObj ps = Left ("invalid close obj: " ++ show ps)

closeArray :: ParseState -> Either String ParseState
closeArray ps@ParseState { nest = Array:ns } = Right (ps { nest = ns })
closeArray ps = Left ("invalid close array: " ++ show ps)

parse :: ParseState -> Char -> Either String ParseState
parse ps c = fmap (\p -> p { idx = idx p + 1 }) (parseC c ps)

incRedGood :: ParseState -> Either a ParseState
incRedGood ps = Right (ps { redGood = redGood ps + 1 })

parseC :: Char -> ParseState -> Either String ParseState
-- in string
parseC c ps@ParseState { inString = True } = case (c, ps) of
  ('r', _) | redStart ps + 1 == idx ps -> incRedGood ps
  ('e', _) | redStart ps + 2 == idx ps -> incRedGood ps
  ('d', _) | redStart ps + 3 == idx ps -> incRedGood ps
  -- "red"
  ('"', ParseState { nest = Object _:ns, redGood = 3 }) | redStart ps + 4 == idx ps ->
    -- assumes that there is never a key "red"
    Right (ps { nest = Object True:ns, inString = False })
  -- any other close quote
  ('"', _) -> Right (ps { inString = False })
  _ -> Right ps
-- not in string
parseC '{' ps = Right (pushNest (Object False) ps { objSum = 0:objSum ps })
parseC '}' ps = parseNum ps >>= closeObj
parseC '[' ps = Right (pushNest Array ps)
parseC ']' ps = parseNum ps >>= closeArray
parseC ',' ps = parseNum ps
parseC '"' ps = Right (ps { inString = True, redGood = 0, redStart = idx ps })
parseC c ps | c == '-' || isDigit c = Right (ps { numBuf = c:numBuf ps })
parseC ':' ps = Right ps
parseC '\n' ps = Right ps
parseC c _ = Left ("invalid char: " ++ [c])

main :: IO ()
main = do
  s <- readInputFile
  case sums s of
    Right (a, b) -> print a >> print b
    Left err -> putStrLn err
