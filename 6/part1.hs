import Data.String
import Data.List
import Text.Regex
import Data.Matrix
import AdventCommon

type Pos = (Int,Int)
type Field = Matrix Int

main = do
  reverseLines <- readLines
  let lines = reverse reverseLines
  let field = foldl' calcInstruction emptyField lines
  print $ length $ filter (==1) $ toList field

emptyField :: Field
emptyField = zero 1000 1000

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
doInstruction instr lights [x1,y1,x2,y2] = foldl instr lights [ (x+1,y+1) | x <- [x1..x2], y <- [y1..y2]]

turnOn field pos = setElem 1 pos field
turnOff field pos = setElem 0 pos field
toggle field pos = setElem value pos field
  where 
    value | field ! pos == 0 = 1
          | field ! pos == 1 = 0
