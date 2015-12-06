import Data.String
import Data.List
import Text.Regex
import qualified Data.Map.Strict as M
import AdventCommon

type Pos = (Int,Int)
type Field = M.Map Pos Int

main = do
  reversedLines <- readLines
  let lines = reverse reversedLines
  let field = foldl' calcInstruction emptyField lines
  print $ M.foldr (+) 0 field

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

turnOn field pos = M.alter f pos field
  where
    f Nothing = Just 1
    f (Just a) = Just $ a+1
turnOff field pos = M.alter f pos field
  where
    f Nothing = Nothing
    f (Just 0) = Just 0
    f (Just a) = Just $ a-1
toggle field pos = M.alter f pos field
  where 
    f Nothing = Just 2
    f (Just a) = Just $ a+2