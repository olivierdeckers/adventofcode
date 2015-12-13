import Text.JSON
import Debug.Trace

main = do
  encoded <- readFile "input.txt"
  solution encoded

solution str = do 
  let (Ok json) = (decode str :: Result JSValue)
  --print $ containsRed json
  let integers = getIntegers json
  print $ sum integers

getIntegers :: JSValue -> [Int]
getIntegers (JSArray []) = []
getIntegers (JSArray (x:xs)) = (getIntegers x) ++ (getIntegers (JSArray xs))
getIntegers JSNull = []
getIntegers (JSBool _) = []
getIntegers (JSRational _ x) = [truncate $ fromRational x]
getIntegers (JSString s) = []
getIntegers (JSObject obj) = result
  where
    (_, values) = unzip $ fromJSObject obj
    result = if (containsRed values) then 
      []
    else
      getIntegers (JSArray values)


containsRed :: [JSValue] -> Bool
containsRed list = any containsRed' list
  where
    containsRed' (JSString s) | fromJSString s == "red" = True
    containsRed' _ = False