import qualified Data.Map.Strict as Map
import Data.Char
import Data.List.Split

type Pos = (Int,Int)
type Plan = (Map.Map Pos Int)
type State = (Pos, Pos, Plan)

main = do
  str <- getLine
  let plan = Map.singleton (0,0) 1 :: Plan
  let initialState = ((0,0), (0,0), plan) :: State
  let (_,_, finalPlan) = foldl move initialState (chunk 2 str)
  putStr $ show $ Map.size finalPlan

move :: State -> [Char] -> State
move (p1, p2, plan) [dir1, dir2] = (p1', p2', plan'')
  where 
    p1' = newPos p1 dir1
    p2' = newPos p2 dir2
    plan' = Map.alter incVisits p1' plan
    plan'' = Map.alter incVisits p2' plan'

incVisits :: Maybe Int -> Maybe Int
incVisits Nothing = Just 1
incVisits (Just x) = Just $ x+1

newPos (x,y) '^' = (x, y+1)
newPos (x,y) '<' = (x-1, y)
newPos (x,y) '>' = (x+1, y)
newPos (x,y) 'v' = (x, y-1)
