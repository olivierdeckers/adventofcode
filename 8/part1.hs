import Data.List
import AdventCommon

main = do
  lines <- readLines
  --let lines = reverse reverseLines
  let lineLengths = sum $ map length lines
  let lineSizes = sum $ map lineSize lines
  print $ lineLengths - lineSizes

lineSize :: String -> Int
lineSize "" = 0
lineSize ('"':cs) = lineSize cs 
lineSize ('\\':'"':cs) = 1 + lineSize cs
lineSize ('\\':'\\':cs) = 1 + lineSize cs
lineSize ('\\':'x':_:_:cs) = 1 + lineSize cs
lineSize (_:cs) = 1 + lineSize cs