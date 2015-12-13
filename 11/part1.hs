import Data.Digits
import Data.Char
import Data.Maybe

solution str = head $ catMaybes $ map intToPassword [start..]
  where
    start = 1 + (passwordToInt str)

intToPassword :: Int -> Maybe String
intToPassword x = letters
  where
    base26 = digits 27 x
    conditions = [hasThreeConsecutive, hasNoConfusingChars, hasTwoNonOverlappingPairs]
    letters = if (elem 0 base26) || (null base26) then
      Nothing
    else if (any (\f -> not $ f base26) conditions) then
      Nothing
    else
      Just $ map (\x -> chr (x+96)) base26

passwordToInt :: String -> Int
passwordToInt pwd = result
  where
    chars = map (\c -> (ord c) - 96) pwd
    result = unDigits 27 chars

hasThreeConsecutive :: [Int] -> Bool
hasThreeConsecutive l | length l < 3 = False
hasThreeConsecutive (x:y:z:rest)
  | x+1 == y && y+1 == z = True
  | otherwise = hasThreeConsecutive (y:z:rest)

hasNoConfusingChars :: [Int] -> Bool
hasNoConfusingChars digits
  | elem 9 digits = False -- I
  | elem 15 digits = False -- O
  | elem 12 digits = False -- l
  | otherwise = True

hasTwoNonOverlappingPairs :: [Int] -> Bool
hasTwoNonOverlappingPairs digits = (countPairs 0 digits) >= 2
  where
    countPairs pairs [] = pairs
    countPairs pairs [_] = pairs
    countPairs pairs (a:b:as)
      | a == b = countPairs (pairs+1) as
      | otherwise = countPairs pairs (b:as)
