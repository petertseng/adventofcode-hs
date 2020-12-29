import AdventOfCode (readInputFile)

import Text.JSON (JSValue(..), JSObject, Result(..), decode, fromJSObject, toJSString)

sumJS :: (JSValue -> Bool) -> JSValue -> Double
sumJS admit j | not (admit j) = 0
sumJS _ JSNull = 0
sumJS _ (JSBool _) = 0
sumJS _ (JSRational _ n) = fromRational n
sumJS _ (JSString _) = 0
sumJS admit (JSArray vs) = sum (map (sumJS admit) vs)
sumJS admit (JSObject obj) = sum (map (sumJS admit) (vals obj))

vals :: JSObject a -> [a]
vals obj = map snd (fromJSObject obj)

hasNoRed :: JSValue -> Bool
hasNoRed (JSObject obj) = red `notElem` vals obj
  where red = JSString (toJSString "red")
hasNoRed _ = True

main :: IO ()
main = do
  s <- readInputFile
  let d = case decode s of
        Ok v -> v
        Error e -> error ("invalid json: " ++ e)
  print (round (sumJS (const True) d))
  print (round (sumJS hasNoRed d))
