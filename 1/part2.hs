import Data.String (String)
import Data.List (findIndex)
import Control.Monad (liftM2)

main :: IO ()
main = do
  str <- getLine
  let Just a = step1 str
  print a

step1 :: String -> Maybe Int
step1 str = liftM2 (+) (Just 1) $ findIndex (<0) $ tail levels
  where 
    levels = scanl f 0 str
    f :: Int -> Char -> Int
    f a '(' = a+1
    f a ')' = a-1
