import Data.String (String)
import Data.List (findIndex)
import Control.Monad (liftM2)

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
