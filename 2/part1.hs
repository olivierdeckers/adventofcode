import Data.List
import Data.String
import Data.List.Split


main = do
  lines <- readLines []
  let dims = map getDimensions lines
  let areas = map (\[a,b,c] -> surfaceArea a b c) dims
  print $ sum areas

getDimensions :: String -> [Int]
getDimensions str = map read $ splitOn "x" str

readLines :: [String] -> IO [String]
readLines strs = do
  str <- getLine
  if length str == 0 then
    return strs
  else 
    readLines (str : strs)

surfaceArea w l h = a1 + a2 + a3 + a4
  where
    a1 = 2*l*w
    a2 = 2*w*h
    a3 = 2*l*h
    a4 = min a1 (min a2 a3) `div` 2

--splitAt :: String -> Char -> [String]
--splitAt "" _ = []
--splitAt str c = splitAt' "" str c
--  where
--    splitAt' acc "" _ = [reverse acc]
--    splitAt' acc (x:xs) c | x == c = (reverse acc) : splitAt' "" xs x 
--    splitAt' acc (x:xs) c | True = splitAt' (x:acc) xs c