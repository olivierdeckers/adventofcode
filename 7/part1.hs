import Data.String
import Data.List
import Text.Regex
import qualified Data.Map.Strict as M
import AdventCommon
import Data.Bits
import Data.Word
import Debug.Trace

main = do
  reversedLines <- readLines
  let lines = reverse reversedLines
  let instructions = foldl' parseInstruction M.empty lines
  print instructions
  print $ eval instructions (Signal (String "a"))

test = do 
  let input = ["123 -> x", "456 -> y", "x AND y -> d", "x OR y -> e", "x LSHIFT 2 -> f", "y RSHIFT 2 -> g", "NOT x -> h", "NOT y -> i"]
  let instructions = foldl parseInstruction M.empty input
  print instructions
  print $ eval instructions (Signal (String "f"))

  
data Signal = String String | Word Word16 deriving Show
data Instruction = Signal Signal | And Signal Signal | Or Signal Signal | Not Signal | LShift Signal Int | RShift Signal Int deriving (Show)

parseInstruction :: M.Map String Instruction -> String -> M.Map String Instruction
parseInstruction instructions str
  | Just [a, b] <- matchRegex assignR str = M.insert b (Signal (parseSignal a)) instructions
  | Just [a, b, c] <- matchRegex andR str = M.insert c (And (parseSignal a) (parseSignal b)) instructions
  | Just [a, b, c] <- matchRegex orR str = M.insert c (Or (parseSignal a) (parseSignal b)) instructions
  | Just [a, b] <- matchRegex notR str = M.insert b (Not $ parseSignal a) instructions
  | Just [a, b, c] <- matchRegex lshiftR str = M.insert c (LShift (parseSignal a) (read b)) instructions
  | Just [a, b, c] <- matchRegex rshiftR str = M.insert c (RShift (parseSignal a) (read b)) instructions
  where
    assignR = mkRegex "^([a-z0-9]+) -> ([a-z]+)$"
    andR = mkRegex "^([a-z0-9]+) AND ([a-z0-9]+) -> ([a-z]+)$"
    orR = mkRegex "^([a-z0-9]+) OR ([a-z0-9]+) -> ([a-z]+)$"
    notR = mkRegex "^NOT ([a-z]+) -> ([a-z]+)$"
    lshiftR = mkRegex "^([a-z0-9]+) LSHIFT ([0-9]+) -> ([a-z]+)$"
    rshiftR = mkRegex "^([a-z0-9]+) RSHIFT ([0-9]+) -> ([a-z]+)$"

parseSignal :: String -> Signal
parseSignal str
  | Just [] <- matchRegex string str = String str
  | Just [] <- matchRegex word str = Word (read str)
  where
    string = mkRegex "[a-z]+"
    word = mkRegex "[0-9]+"

eval :: M.Map String Instruction -> Instruction -> Word16
eval instructions (Signal (Word a)) = a
eval instructions (Signal (String a)) = trace (a ++ ": " ++ (show result)) result
  where 
    Just contents = M.lookup a instructions 
    result = eval instructions contents
eval instructions (And a b) = trace (show (And a b)) $ (eval instructions (Signal a)) .&. (eval instructions (Signal b))
eval instructions (Or a b) = trace (show (Or a b)) $ (eval instructions (Signal a)) .|. (eval instructions (Signal b))
eval instructions (Not a) = trace (show (Not a)) $ complement (eval instructions (Signal a))
eval instructions (LShift a b) = trace (show (LShift a b)) $ shiftL (eval instructions (Signal a)) b
eval instructions (RShift a b) = trace (show (RShift a b)) $ shiftR (eval instructions (Signal a)) b