import Text.JSON

main = do
  encoded <- readFile "input.txt"
  let (Ok json) = (decode encoded :: Result JSValue)
  let integers = getIntegers json
  print $ sum integers

getIntegers :: JSValue -> [Int]
getIntegers (JSArray []) = []
getIntegers (JSArray (x:xs)) = (getIntegers x) ++ (getIntegers (JSArray xs))
getIntegers JSNull = []
getIntegers (JSBool _) = []
getIntegers (JSRational _ x) = [truncate $ fromRational x]
getIntegers (JSString s) = []
getIntegers (JSObject obj) = getIntegers (JSArray values)
  where
    (_, values) = unzip $ fromJSObject obj
