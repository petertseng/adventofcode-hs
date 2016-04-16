{-# LANGUAGE ExistentialQuantification #-}

import Data.Either (partitionEithers)
import Data.List (maximumBy, minimumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import System.Environment (getArgs)
import Text.Read (readMaybe)

-- Gratuitous work with types so that we never mix equipment types...
-- unless we want to (using Equipment)!
-- This is obviously unnecessary.
-- However, I wrote it to explore what types can do for me.

-- From https://wiki.haskell.org/Heterogenous_collections
-- Some would argue however that this is unnatural Haskell, such as
-- http://stackoverflow.com/questions/12774056/
data Equippable = forall a. Equipment a => Equippable a

equip :: Equipment a => a -> Equippable
equip = Equippable

data Sword = Sword Int Int deriving Show
data Armor = Armor Int Int deriving Show
data Ring  = DamageRing Int Int | ArmorRing Int Int deriving (Eq, Show)

type Setup = (Sword, Armor, Ring, Ring)

-- It's very unfortunate that we have to prefix the names.
data Combatant = Combatant { hp :: Int
                           , cDamage :: Int
                           , cArmor :: Int
                           }

class Equipment a where
  cost   :: a -> Int
  damage :: a -> Int
  armor  :: a -> Int

instance Equipment Sword where
  cost   (Sword c _) = c
  damage (Sword _ d) = d
  armor  _           = 0

instance Equipment Armor where
  cost   (Armor c _) = c
  damage _           = 0
  armor  (Armor _ a) = a

instance Equipment Ring where
  cost   (DamageRing c _) = c
  cost   (ArmorRing c _)  = c
  damage (DamageRing _ d) = d
  damage (ArmorRing _ _)  = 0
  armor  (DamageRing _ _) = 0
  armor  (ArmorRing _ a)  = a

swords :: [Sword]
swords = [ Sword 8 4
         , Sword 10 5
         , Sword 25 6
         , Sword 40 7
         , Sword 74 8
         ]

armors :: [Armor]
armors = [ Armor 0 0
         , Armor 13 1
         , Armor 31 2
         , Armor 53 3
         , Armor 75 4
         , Armor 102 5
         ]

rings :: [Ring]
rings = [ DamageRing 0 0
        , DamageRing 25 1
        , DamageRing 50 2
        , DamageRing 100 3
        , ArmorRing 20 1
        , ArmorRing 40 2
        , ArmorRing 80 3
        ]

turnsToKill :: Combatant -> Combatant -> Int
attacker `turnsToKill` defender =
  ceiling (fromIntegral (hp defender) / perTurn)
  where perTurn = max 1.0 (fromIntegral effectiveDamage) :: Double
        effectiveDamage = cDamage attacker - cArmor defender

winner :: Combatant -> Combatant -> a -> Either a a
winner boss hero = if heroTurns <= bossTurns then Right else Left
  where heroTurns = hero `turnsToKill` boss
        bossTurns = boss `turnsToKill` hero

result :: Combatant -> Setup -> Either (Int, Setup) (Int, Setup)
result boss es@(s, a, r1, r2) = winner boss hero (sumBy cost', es)
  where equips = [equip s, equip a, equip r1, equip r2]
        hero = Combatant 100 heroDamage heroArmor

        heroDamage = sumBy damage'
        heroArmor  = sumBy armor'

        sumBy f = sum (map f equips)

        cost'   (Equippable e) = cost e
        damage' (Equippable e) = damage e
        armor'  (Equippable e) = armor e

printResult :: (Int, Setup) -> IO ()
--printResult (c, s) = print c >> print s
printResult (c, _) = print c

findNumbers :: String -> [Int]
findNumbers = mapMaybe readMaybe . words

main :: IO ()
main = do
  args <- getArgs
  boss <- case args of
    a:b:c:_ -> return (Combatant (read a) (read b) (read c))
    f:_ -> readFile f >>= (\s -> let [a, b, c] = findNumbers s in return (Combatant a b c))
    _ -> readFile "/dev/stdin" >>= (\s -> let [a, b, c] = findNumbers s in return (Combatant a b c))
  let setups = [(s, a, r1, r2)
               | s <- swords, a <- armors, r1 <- rings, r2 <- rings
               -- No duplicate rings, unless it's the dummy ring.
               , r1 == DamageRing 0 0 || r1 /= r2]
      (losses, wins) = partitionEithers (map (result boss) setups)
  printResult (minimumBy (comparing fst) wins)
  printResult (maximumBy (comparing fst) losses)
