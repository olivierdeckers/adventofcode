

expand :: String -> String
expand x = expand' Nothing x
  where
    expand' :: (Maybe (Char, Int)) -> String -> String
    expand' Nothing "" = ""
    expand' Nothing (c:cs) = expand' (Just (c, 1)) cs
    expand' (Just (x,n)) "" = (show n) ++ [x]
    expand' (Just (x,n)) (c:cs)
      | x == c = expand' (Just (x, n+1)) cs
      | otherwise = (show n) ++ [x] ++ (expand' Nothing (c:cs))