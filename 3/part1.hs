import qualified Data.Map.Strict as Map
import Data.Char

type Pos = (Int,Int)
type Plan = (Map.Map (Int,Int) Int)
type State = (Pos, Plan)

main = do
  str <- getLine
  let plan = Map.singleton (0,0) 1 :: Plan
  let initialState = ((0,0), plan) :: State
  let (_, finalPlan) = foldl move initialState str
  putStr $ show $ Map.size finalPlan

move :: State -> Char -> State
move (p1, plan) dir = (p1', plan')
  where 
    p1' = newPos p1 dir
    plan' = Map.alter incVisits p1' plan

incVisits :: Maybe Int -> Maybe Int
incVisits Nothing = Just 1
incVisits (Just x) = Just $ x+1

newPos (x,y) '^' = (x, y+1)
newPos (x,y) '<' = (x-1, y)
newPos (x,y) '>' = (x+1, y)
newPos (x,y) 'v' = (x, y-1)
