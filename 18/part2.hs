

main = do
  l <- readFile "input2.txt"
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

stuckLights size = [(0,0),(0,size),(size,0),(size,size)]

getLightState field (x,y) = (field !! y) !! x

nextField field = map nextRow [0..size]
  where
    size = (length field) - 1
    nextCell pos
      | elem pos (stuckLights size) = True
      | getLightState field pos == False = (length $ filter id $ neighbours pos field) == 3
      | getLightState field pos == True = elem (length $ filter id $ neighbours pos field) [2,3]
    nextRow y = map nextCell [(x,y) | x <- [0..size]]

countLights :: [[Bool]] -> Int
countLights field = sum $ map (length . (filter id)) field