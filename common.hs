module AdventCommon (readLines) where

readLines :: IO [String]
readLines = readLines' []
  where
    readLines' :: [String] -> IO [String]
    readLines' strs = do
      str <- getLine
      if length str == 0 then
        return strs
      else 
        readLines' (str : strs)