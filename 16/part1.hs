import Text.Regex
import Control.Monad

main = do
  contents <- readFile "input.txt"
  let ls = lines contents
  let aunts = map parseAunt ls
  let solution = filter matchesResult aunts
  print solution

parseAunt l = (sue, (map (liftM (read . head)) properties))
  where
    Just sue = matchRegex (mkRegex "Sue ([0-9]+):") l
    children = matchRegex (mkRegex "children: ([0-9]+)") l
    cats = matchRegex (mkRegex "cats: ([0-9]+)") l
    samoyeds = matchRegex (mkRegex "samoyeds: ([0-9]+)") l
    pomeranians = matchRegex (mkRegex "pomeranians: ([0-9]+)") l
    akitas = matchRegex (mkRegex "akitas: ([0-9]+)") l
    vizslas = matchRegex (mkRegex "vizslas: ([0-9]+)") l
    goldfish = matchRegex (mkRegex "goldfish: ([0-9]+)") l
    trees = matchRegex (mkRegex "trees: ([0-9]+)") l
    cars = matchRegex (mkRegex "cars: ([0-9]+)") l
    perfumes = matchRegex (mkRegex "perfumes: ([0-9]+)") l
    properties = [children, cats, samoyeds, pomeranians, akitas, vizslas, goldfish, trees, cars, perfumes]

matchesResult (_, properties) = all matches pairs
  where
    result = [3,7,2,3,0,0,5,3,2,1]
    pairs = zip properties result
    matches (Nothing,_) = True
    matches (Just a, b) = a == b