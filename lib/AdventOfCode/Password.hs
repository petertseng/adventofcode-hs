module AdventOfCode.Password (hasSafePair, nextPair, nextPassword) where

succNoConfuse :: Char -> Char
succNoConfuse 'h' = 'j'
succNoConfuse 'n' = 'p'
succNoConfuse 'k' = 'm'
succNoConfuse c = succ c

-- Assumes passwords are reversed.
-- It's much easier to work off the head of a list.
nextNoConfuse :: String -> String
nextNoConfuse s = as ++ nextRest
  where as = replicate (length zs) 'a'
        (zs, rest) = span (== 'z') s
        nextRest = case rest of
          h:t -> succNoConfuse h : t
          [] -> []

-- Next password with a pair
nextPair :: String -> String
nextPair s@(h:_) = case nextNoConfuse s of
  -- cbz -> cca, but NOT aaa -> aab
  s'@(_:b:c:_) | h == 'z' && b == c -> s'

  -- We don't need to check more than 3 chars.
  -- The only way >= 3 chars change is if at least the last two chars are z.
  -- In that case, nextNoConfuse would give a string ending in aa, and that's a pair.
  -- For the same reason, it's OK that we didn't return early for dczz -> ddaa.

  -- Easy case: Only the last char changes.
  -- For example, cba -> cbb
  a:b:t | b > a -> b : b : t

  -- The third-to-last and second-to-last character make a pair.
  -- For example, cbc -> cca (which comes before ccc)
  (a:b:c:t) | b < a && c == succNoConfuse b -> 'a' : c : c : t

  -- The second-to-last and last character make a pair.
  -- For example, cab -> cbb (which comes before cca)
  (a:b:t) | b < a -> let b' = succNoConfuse b in b' : b' : t

  -- nextNoConfuse already made (a:b:t) where a == b, which is a pair.
  s' -> s'

nextPair s = error ("not enough chars in password " ++ s)

-- Next candidate password, attempting to keep pairs.
nextCandidate :: String -> String
nextCandidate s | hasPairs s    = nextNoConfuse s
nextCandidate s | hasSafePair s = nextPair s
nextCandidate (_:_:t)           = "aa" ++ nextPair t
nextCandidate s = error ("not enough chars in password " ++ s)

hasStraight :: String -> Bool
hasStraight = has ('\0', '\0')
  where has (a, b) (x:_) | a == succ b && b == succ x = True
        has (_, b) (x:xs) = has (b, x) xs
        has _ [] = False

hasPairs :: String -> Bool
hasPairs = has1 '\0'
  where has1 a (x:xs) | a == x = has2 '\0' xs
        has1 _ (x:xs) = has1 x xs
        has1 _ [] = False
        has2 a (x:xs) = a == x || has2 x xs
        has2 _ [] = False

-- A safe pair (in a reversed password)
-- is one that will not be destroyed when searching for another.
hasSafePair :: String -> Bool
hasSafePair (_:_:s) = has '\0' s
  where has a (x:xs) = a == x || has x xs
        has _ [] = False
hasSafePair _ = False

goodPassword :: String -> Bool
goodPassword s = hasPairs s && hasStraight s

nextPassword :: String -> String
nextPassword p = reverse (until1 goodPassword nextCandidate (reverse p))

until1 :: (a -> Bool) -> (a -> a) -> a -> a
until1 p f x = until p f (f x)
