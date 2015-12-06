
import Data.String
import AdventCommon

main = do
  strs <- readLines
  let niceStrings = filter niceString strs
  print $ length niceStrings

niceString str = and $ map (\f -> f str) conditions
  where
    conditions = [hasThreeVowels, hasDoubleLetters, noEvilSubstrings]


hasThreeVowels str = length vowels >= 3
  where
    vowels = filter vowel str
    vowel 'a' = True
    vowel 'e' = True
    vowel 'i' = True
    vowel 'o' = True
    vowel 'u' = True
    vowel _ = False

hasDoubleLetters [] = False
hasDoubleLetters [c] = False
hasDoubleLetters (c:c2:cs)
  | c == c2 = True
  | otherwise = hasDoubleLetters (c2:cs)

noEvilSubstrings [] = True
noEvilSubstrings [c] = True
noEvilSubstrings (c:c2:cs)
  | [c,c2] == "ab" = False
  | [c,c2] == "cd" = False
  | [c,c2] == "pq" = False
  | [c,c2] == "xy" = False
  | otherwise = noEvilSubstrings (c2:cs)