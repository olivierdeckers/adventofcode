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

surfaceArea :: Int -> Int -> Int -> Int
surfaceArea w l h = 2*s1 + 2*s2 + w * l * h
  where
    s1 = minimum [w,l,h]
    rest = delete s1 [w,h,l]
    s2 = minimum rest

--splitAt :: String -> Char -> [String]
--splitAt "" _ = []
--splitAt str c = splitAt' "" str c
--  where
--    splitAt' acc "" _ = [reverse acc]
--    splitAt' acc (x:xs) c | x == c = (reverse acc) : splitAt' "" xs x 
--    splitAt' acc (x:xs) c | True = splitAt' (x:acc) xs c