{-# LANGUAGE FlexibleContexts #-}

import Control.Monad (forM_, when)
import Control.Monad.State (State, evalState, get, put)
import Data.List (delete, minimumBy)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (comparing)
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Read (readMaybe)

data Spell = Poison | Shield | Recharge | MagicMissile | Drain deriving Show

spells :: [Spell]
spells = [Poison, Shield, Recharge, MagicMissile, Drain]

cost :: Spell -> Int
cost Poison = 173
cost Shield = 113
cost Recharge = 229
cost MagicMissile = 53
cost Drain = 73

data Game = Game { myHp :: Int
                 , myMp :: Int
                 , bossHp :: Int
                 , bossDamage :: Int
                 , hard :: Bool
                 , shieldTime :: Int
                 , poisonTime :: Int
                 , rechargeTime :: Int
                 } deriving Show

type GameState = (Int, Int, Int, Int, Int, Int)

legal :: Game -> Spell -> Bool
legal g s | cost s > myMp g = False
legal g Poison = poisonTime g <= 0
legal g Shield = shieldTime g <= 0
legal g Recharge = rechargeTime g <= 0
legal _ MagicMissile = True
legal _ Drain = True

gameState :: Game -> GameState
gameState g = (myHp g, myMp g, bossHp g, shieldTime g, poisonTime g, rechargeTime g)

type SearchResult = Maybe (Int, [Spell])

search :: Game -> [Spell] -> Int -> State (Maybe Int, Map.Map GameState Int) SearchResult
search g spellsSoFar costSoFar = do
  (best, seen) <- get
  case (best, Map.lookup (gameState g) seen) of
    (_, Just prevCost) | prevCost <= costSoFar -> return Nothing
    (Just bestCost, _) | bestCost <= costSoFar -> return Nothing
    _ -> do
      put (best, Map.insert (gameState g) costSoFar seen)
      let legalSpells = filter (legal g) spells
      if null legalSpells
        then return Nothing
        else bestOf legalSpells
  where
    bestOf legalSpells = do
      results <- mapM try legalSpells
      let successes = catMaybes results
      if null successes
        then return Nothing
        else return (Just (minimumBy (comparing fst) successes))
    try spell = do
      let spellsSoFar' = spell : spellsSoFar
      let costSoFar' = cost spell + costSoFar
      case castSpell g spell of
        Right g'   -> search g' spellsSoFar' costSoFar'
        Left False -> return Nothing
        Left True  -> do
          (best, seen) <- get
          case best of
            Nothing -> put (Just costSoFar', seen)
            Just v  -> put (Just (min v costSoFar'), seen)
          return (Just (costSoFar', reverse spellsSoFar'))

winner :: Game -> Either Bool Game
winner g | myHp g <= 0   = Left False
winner g | bossHp g <= 0 = Left True
winner g                 = Right g

castSpell :: Game -> Spell -> Either Bool Game
castSpell g s =
  applySpellEffect g s >>= tickTimers >>=
  bossAttack >>= hardModePenalty >>= tickTimers >>=
  winner

-- In general, I would like a function that's something like:
-- scanM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> [m a]
-- But this is not possible in general.
-- If scanM existed, its type is:
-- scanM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m [a]
-- Notice that you can't flip the m and [] monads.
scanSpells :: Game -> [Spell] -> [Either Bool Game]
scanSpells g [] = [Right g]
scanSpells g (spell:rest) = case castSpell g spell of
  Left l -> [Right g, Left l]
  Right g' -> Right g : scanSpells g' rest

applySpellEffect :: Game -> Spell -> Either Bool Game
applySpellEffect g s | cost s > myMp g =
  error (printf "Can't cast %s in %s" (show s) (show g))
applySpellEffect g s = winner g' { myMp = myMp g - cost s }
  where g' = case s of
          MagicMissile -> g { bossHp = bossHp g - 4 }
          Drain -> g { bossHp = bossHp g - 2, myHp = myHp g + 2 }
          Shield -> g { shieldTime = 6 }
          Poison -> g { poisonTime = 6 }
          Recharge -> g { rechargeTime = 5 }

bossAttack :: Game -> Either Bool Game
bossAttack g = winner g { myHp = myHp g - damage }
  where damage = max 1 (bossDamage g - armor)
        armor = if shieldTime g > 0 then 7 else 0

hardModePenalty :: Game -> Either Bool Game
hardModePenalty g = winner g'
  where g' = if hard g then g { myHp = myHp g - 1 } else g

tickTimers :: Game -> Either Bool Game
tickTimers g0 = winner g3
  where g1 = if shieldTime g0 > 0
               then g0 { shieldTime = shieldTime g0 - 1 }
               else g0
        g2 = if rechargeTime g1 > 0
               then g1 { rechargeTime = rechargeTime g1 - 1, myMp = myMp g1 + 101 }
               else g1
        g3 = if poisonTime g2 > 0
               then g2 { poisonTime = poisonTime g2 - 1, bossHp = bossHp g2 - 3 }
               else g2

exampleGame :: Int -> Game
exampleGame hp = Game { myHp = 10
                      , myMp = 250
                      , bossHp = hp
                      , bossDamage = 8
                      , hard = False
                      , shieldTime = 0
                      , poisonTime = 0
                      , rechargeTime = 0
                      }

printResult :: SearchResult -> IO ()
printResult Nothing = putStrLn "Can't win"
--printResult (Just (mp, moves)) = print mp >> print moves
printResult (Just (mp, _)) = print mp

findNumbers :: String -> [Int]
findNumbers = mapMaybe readMaybe . words

main :: IO ()
main = do
  args <- getArgs
  (hp, damage) <- case delete "-d" args of
    a:b:_ -> return (read a, read b)
    f:_ -> readFile f >>= (\s -> let [a, b] = findNumbers s in return (a, b))
    _ -> readFile "/dev/stdin" >>= (\s -> let [a, b] = findNumbers s in return (a, b))
  let debug = "-d" `elem` args
      game h = Game { myHp = if h then 49 else 50
                    , myMp = 500
                    , bossHp = hp
                    , bossDamage = damage
                    , hard = h
                    , shieldTime = 0
                    , poisonTime = 0
                    , rechargeTime = 0
                    }

  when debug $ do
    mapM_ print (scanSpells (exampleGame 13) [Poison, MagicMissile])
    mapM_ print (scanSpells (exampleGame 14) [Poison, MagicMissile])
    mapM_ print (scanSpells (exampleGame 14) [Recharge, Shield, Drain, Poison, MagicMissile])

  forM_ [False, True] $ \h ->
    printResult (evalState (search (game h) [] 0) (Nothing, Map.empty))
