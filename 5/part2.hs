import Data.String
import Data.List
import AdventCommon

main = do
  strs <- readLines
  let niceStrings = filter niceString strs
  print $ length niceStrings

niceString str = and $ map (\f -> f str) conditions
  where
    conditions = [containsDoublePair, containsRepeatingLetter]

containsDoublePair [] = False
containsDoublePair [_] = False
containsDoublePair (c:c2:cs) 
  | isInfixOf [c,c2] cs = True
  | otherwise = containsDoublePair (c2:cs)

containsRepeatingLetter str | length str < 3 = False
containsRepeatingLetter (c:c2:c3:cs)
  | c == c3 = True
  | otherwise = containsRepeatingLetter (c2:c3:cs)