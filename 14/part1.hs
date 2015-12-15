import Data.List

--reindeer = [((0,0), 14, 10, 127), ((0,0), 16, 11, 162)]
reindeer = [
	((0,0), 19, 7, 124), 
	((0,0), 3, 15, 28),
	((0,0), 19, 9, 164),
	((0,0), 19, 9, 158),
	((0,0), 13, 7, 82),
	((0,0), 25, 6, 145),
	((0,0), 14, 3, 38),
	((0,0), 3, 16, 37),
	((0,0), 25, 6, 143)
	]

endPos r tMax = position
  where
    Just index = findIndex (\((p,t),_,_,_) -> t > tMax) $ simulate r
    ((position,_),_,_,_) = (simulate r) !! (index)
simulate r = iterate simulateStep r
simulateStep ((p,t), vel, end, rest) = ((p+vel*end, t+end+rest), vel, end, rest)

solution = map (flip endPos 2503) reindeer
