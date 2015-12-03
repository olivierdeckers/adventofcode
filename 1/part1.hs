import Data.String (String)

main :: IO ()
main = do
  str <- getLine
  print $ step1 str

step1 :: String -> Int
step1 str = foldl f 0 str
  where
  f :: Int -> Char -> Int
  f a '(' = a+1
  f a ')' = a-1

step1' :: String -> Int
step1' s = sum [if x == '(' then 1 else -1 | x <- s]