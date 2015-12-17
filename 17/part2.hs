
--containers = [20, 15, 10, 5, 5]
--solution = store 25 containers

main = do
  contents <- readFile "input.txt"
  let containers = map read $ lines contents
  let ways = store 150 containers
  let minways = minimum $ map length ways
  print $ length $ filter (\x -> length x == minways) ways


store :: Int -> [Int] -> [[Int]]
store 0 _ = [[]]
store _ [] = []
store x _ | x < 0 = []
store amt (container:containers) = ways1 ++ ways2
  where 
    ways1 = map (container:) $ store (amt-container) containers
    ways2 = store amt containers