

main = do
  l <- readFile "input.txt"
  let ls = lines l
  let field = map parseLine ls
  print $ countLights $ (iterate nextField field) !! 100
  return field

parseLine = map f
  where
    f '#' = True
    f '.' = False 

neighbours (x,y) field = map (getLightState field) neighbourPos
  where
    neighbourPos = filter inBounds $ [
        (x-1,y-1),
        (x,y-1),
        (x+1,y-1),
        (x-1,y),
        (x+1,y),
        (x-1,y+1),
        (x,y+1),
        (x+1,y+1)
      ]
    size = length field
    inBounds (x,y) 
      | x < 0 || y < 0 = False
      | x >= size || y >= size = False
      | otherwise = True

getLightState field (x,y) = (field !! y) !! x

nextField field = map nextRow [0..size-1]
  where
    size = length field
    nextCell pos
      | getLightState field pos == False = (length $ filter id $ neighbours pos field) == 3
      | getLightState field pos == True = elem (length $ filter id $ neighbours pos field) [2,3]
    nextRow y = map nextCell [(x,y) | x <- [0..size-1]]

countLights :: [[Bool]] -> Int
countLights field = sum $ map (length . (filter id)) field