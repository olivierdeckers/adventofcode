import Data.List

distances = [
        [  0,  65, 129, 144,  71, 137,   3, 149], 
        [ 65,   0,  63,   4, 105, 125,  55,  14], 
        [129,  63,   0,  68,  52,  65,  22, 143], 
        [144,   4,  68,   0,   8,  23, 136, 115], 
        [ 71, 105,  52,   8,   0, 101,  84,  96], 
        [137, 125,  65,  23, 101,   0, 107,  14], 
        [  3,  55,  22, 136,  84, 107,   0,  46], 
        [149,  14, 143, 115,  96,  14,  46,   0]]

--distances = [
--      [0, 464, 518],
--      [464, 0, 141],
--      [518, 141, 0]
    --]

distance a b 
  | a > b = (distances !! a) !! b
  | otherwise = distance b a

cities = [0..7]

pathLength (x:xs) = foldl' addPathLength (0, x) xs
  where
    addPathLength (lengthSoFar, prevCity) city = (lengthSoFar + (distance prevCity city), city)

solution = shortestPath
  where
    paths = permutations cities
    lengths = map (\x -> (x, pathLength x)) paths
    shortestPath = minimumBy (\(_, l) (_,l2) -> compare l l2) lengths
