import Data.List
import AdventCommon

main = do
  lines <- readLines
  --let lines = reverse reverseLines
  let lineLengths = sum $ map (length . encodeString) lines
  let lineSizes = sum $ map length lines
  print $ lineLengths - lineSizes

lineSize :: String -> Int
lineSize "" = 0
lineSize ('"':cs) = lineSize cs 
lineSize ('\\':'"':cs) = 1 + lineSize cs
lineSize ('\\':'\\':cs) = 1 + lineSize cs
lineSize ('\\':'x':_:_:cs) = 1 + lineSize cs
lineSize (_:cs) = 1 + lineSize cs

encodeString :: String -> String
encodeString str = '"' : (encodeString' str) ++ "\""
  where
    encodeString' "" = ""
    encodeString' ('"':cs) = '\\' : '"' : (encodeString' cs)
    encodeString' ('\\':cs) = '\\' : '\\' : (encodeString' cs)
    encodeString' (c:cs) = c : (encodeString' cs)