import Data.String
import Data.List
import Text.Regex
import qualified Data.Map.Strict as M
import AdventCommon

type Pos = (Int,Int)
type Field = M.Map Pos Bool

main = do
  lines <- readLines
  let field = foldl calcInstruction emptyField lines
  print $ M.size $ M.filter (==True) field

emptyField :: Field
emptyField = M.empty

calcInstruction :: Field -> String -> Field
calcInstruction lights str
  | Just matches <- matchRegex onRegex str = doInstruction turnOn lights (map read matches)
  | Just matches <- matchRegex offRegex str = doInstruction turnOff lights (map read matches)
  | Just matches <- matchRegex toggleRegex str = doInstruction toggle lights (map read matches)
  where
    onRegex = mkRegex "turn on ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)"
    offRegex = mkRegex "turn off ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)"
    toggleRegex = mkRegex "toggle ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)"

doInstruction :: (Field -> Pos -> Field) -> Field -> [Int] -> Field
doInstruction instr lights [x1,y1,x2,y2] = foldl instr lights [ (x,y) | x <- [x1..x2], y <- [y1..y2]]

turnOn field pos = M.insert pos True field
turnOff field pos = M.insert pos False field
toggle field pos = M.alter f pos field
  where 
    f Nothing = Just True
    f (Just a) = Just $ not a