import Data.List

--Sugar: capacity 3, durability 0, flavor 0, texture -3, calories 2
--Sprinkles: capacity -3, durability 3, flavor 0, texture 0, calories 9
--Candy: capacity -1, durability 0, flavor 4, texture 0, calories 1
--Chocolate: capacity 0, durability 0, flavor -2, texture 2, calories 8

ingredients = [
    [3,0,0,-3,2],
    [-3,3,0,0,9],
    [-1,0,4,0,1],
    [0,0,-2,2,8]
  ]

--utterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
--Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3
--ingredients = [
--    [-1,-2,6,3,8],
--    [2,3,-2,-1,3]
--  ]

evaluateIngredients percentages = foldl' (*) 1 (take 4 minzero)
  where
    ingredientScores = map evaluateIngredient $ zip percentages ingredients
    sums = map sum (transpose ingredientScores)
    minzero = map (max 0) sums

evaluateIngredient (percentage,ingredient) = result
  where
    combined = zip (replicate 5 percentage) ingredient
    result = map (\(a,b) -> a*b) combined

solution = maximum $ map evaluateIngredients percentages
  where
    --percentages = [[a,b] | a <- [0..100], let b = 100-a, b>=0]
    percentages = [[a,b,c,d] | a<-[0..100], b<-[0..100], c<-[0..100], let d=100-a-b-c, d>=0]