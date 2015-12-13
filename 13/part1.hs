import Text.Regex
import qualified Data.Map.Strict as M
import AdventCommon
import Debug.Trace
import Data.List

type Rules = M.Map (String, String) Int

main = do
  ls <- readLines
  let rules = foldl parseHappinessRule M.empty ls
  let configuration = randomConfiguration rules
  let configurations = permutations configuration
  let scores = map (flip evaluateConfiguration rules) configurations
  print $ maximum scores

parseHappinessRule :: Rules -> String -> Rules
parseHappinessRule rules str
  | Just [p1, amount, p2] <- matchRegex gainRegex str = M.insert (p1,p2) (read amount) rules
  | Just [p1, amount, p2] <- matchRegex loseRegex str = M.insert (p1,p2) (negate $ read amount) rules
  | trace str False = rules
  where
    gainRegex = mkRegex "^([a-zA-Z]+) would gain ([0-9]+) happiness units by sitting next to ([a-zA-Z]+).$"
    loseRegex = mkRegex "^([a-zA-Z]+) would lose ([0-9]+) happiness units by sitting next to ([a-zA-Z]+).$"

randomConfiguration rules = nub $ map fst (M.keys rules)

evaluateConfiguration config rules = evaluateConfiguration' (config ++ [head config]) rules
  where 
    evaluateConfiguration' [_] _ = 0
    evaluateConfiguration' (p1:p2:rest) rules = sum [cost1, cost2, cost3]
      where 
        Just cost1 = M.lookup (p1,p2) rules
        Just cost2 = M.lookup (p2,p1) rules
        cost3 = evaluateConfiguration' (p2:rest) rules