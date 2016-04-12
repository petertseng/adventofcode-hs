module AdventOfCode.Password (nextPassword) where

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

goodPassword :: String -> Bool
goodPassword s = hasPairs s && hasStraight s

nextPassword :: String -> String
nextPassword p = reverse (until1 goodPassword nextNoConfuse (reverse p))

until1 :: (a -> Bool) -> (a -> a) -> a -> a
until1 p f x = until p f (f x)
