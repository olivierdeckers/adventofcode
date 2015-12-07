import Data.List
import Text.Regex
import qualified Data.Map.Strict as M
import AdventCommon
import Data.Bits
import Data.Word

main = do
  reversedLines <- readLines
  let lines = reverse reversedLines
  let (instructions, assignments) = foldl' parseInstruction (M.empty, M.empty) lines
  let results = eval (M.toList instructions) assignments
  print $ M.lookup "a" results

test = do 
  let input = ["123 -> x", "456 -> y", "x AND y -> d", "x OR y -> e", "x LSHIFT 2 -> f", "y RSHIFT 2 -> g", "NOT x -> h", "NOT y -> i"]
  let (instructions, assignments) = foldl' parseInstruction (M.empty, M.empty) input
  print $ eval (M.toList instructions) assignments

data Signal = String String | Word Word16 deriving Show
data Instruction = Var String | And Signal Signal | Or Signal Signal | Not Signal | LShift Signal Int | RShift Signal Int deriving (Show)
type Instructions = M.Map String Instruction
type Assignments = M.Map String Word16

parseInstruction :: (Instructions, Assignments) -> String -> (Instructions, Assignments)
parseInstruction (instructions, assignments) str
  | Just [a, b] <- matchRegex assignR str = case (parseSignal a) of
      Word w -> (instructions, M.insert b w assignments)
      String s -> (M.insert b (Var s) instructions, assignments)
  | Just [a, b, c] <- matchRegex andR str = (M.insert c (And (parseSignal a) (parseSignal b)) instructions, assignments)
  | Just [a, b, c] <- matchRegex orR str = (M.insert c (Or (parseSignal a) (parseSignal b)) instructions, assignments)
  | Just [a, b] <- matchRegex notR str = (M.insert b (Not $ parseSignal a) instructions, assignments)
  | Just [a, b, c] <- matchRegex lshiftR str = (M.insert c (LShift (parseSignal a) (read b)) instructions, assignments)
  | Just [a, b, c] <- matchRegex rshiftR str = (M.insert c (RShift (parseSignal a) (read b)) instructions, assignments)
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

eval :: [(String,Instruction)] -> Assignments -> Assignments
eval [] results = results
eval ((key, i):is) results = case i of
  Var s -> 
    case M.lookup s results of
      Just w -> eval is (M.insert key w results)
      Nothing -> eval (is ++ [(key, i)]) results
  Not s -> 
    case resolveSignal s of 
      Just w -> eval is (M.insert key (complement w) results)
      Nothing -> eval (is ++ [(key, i)]) results
  LShift s b ->
    case resolveSignal s of
      Just w -> eval is (M.insert key (shiftL w b) results)
      Nothing -> eval (is ++ [(key, i)]) results
  RShift s b ->
    case resolveSignal s of 
      Just w -> eval is (M.insert key (shiftR w b) results)
      Nothing -> eval (is ++ [(key, i)]) results
  And s1 s2 ->
    case (resolveSignal s1, resolveSignal s2) of
      (Just w1, Just w2) -> eval is (M.insert key (w1 .&. w2) results)
      otherwise -> eval (is ++ [(key, i)]) results
  Or s1 s2 ->
    case (resolveSignal s1, resolveSignal s2) of
      (Just w1, Just w2) -> eval is (M.insert key (w1 .|. w2) results)
      otherwise -> eval (is ++ [(key, i)]) results
  where
    resolveSignal (Word w) = Just w
    resolveSignal (String s) = M.lookup s results
