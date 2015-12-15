import Data.List

--reindeer = [((0,0,10,127), 14, 10, 127), ((0,0,11,162), 16, 11, 162)]
reindeer = [
  ((0,0,7,124), 19, 7, 124), 
  ((0,0,15,28), 3, 15, 28),
  ((0,0,9,164), 19, 9, 164),
  ((0,0,9,158), 19, 9, 158),
  ((0,0,7,82), 13, 7, 82),
  ((0,0,6,145), 25, 6, 145),
  ((0,0,3,38), 14, 3, 38),
  ((0,0,16,37), 3, 16, 37),
  ((0,0,6,143), 25, 6, 143)
  ]

-- simulate step calculates the situation for the next step
simulateStep ((p,t,0,restRemaining), vel, end, rest)
  | restRemaining == 1 = ((p, t+1, end, rest), vel, end, rest)
  | otherwise = ((p, t+1, 0, restRemaining-1), vel, end, rest)

simulateStep ((p,t,endRemaining,restRemaining), vel, end, rest) = 
  ((p+vel, t+1, endRemaining-1, restRemaining), vel, end, rest)

-- simulate simulates the situations until the end of time
simulate r = iterate simulateStep r


solution = let
    -- simulation simulates until t=2503, and transposes, so that we have
    -- a list of situations (list containing position of all reindeers at time t)
    -- intead of a list of reindeer simulations
    simulation = transpose $ map ((take 2503) . tail . simulate) reindeer
    -- We assign points at every time t to the winning reindeers
    points = map assignPoints simulation
    -- We transpose the points to get a list of points per reindeer 
    -- instead of a list of points assignments at time t
    pointsPerReindeer = transpose points
  in
    -- We sum the points for each reindeer and return the winning score
    maximum $ map sum pointsPerReindeer

-- assigns points to all the winning reindeers in a given situation
assignPoints situation = points
  where
    positions = map getPos situation
    winning = maximum positions
    points = map (\p -> if p == winning then 1 else 0) positions

getPos ((p,_,_,_),_,_,_) = p