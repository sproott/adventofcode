import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Data.List.Split (chunksOf)

type Rules = Map String [(Int, String)]

canContainShinyGoldBag :: Rules -> String -> Bool
canContainShinyGoldBag rules bag = 
  let bags = fromJust $ Map.lookup bag rules
  in if "shiny gold" `elem` fmap snd bags 
    then True 
    else any (canContainShinyGoldBag rules) (fmap snd bags)

solve1 :: Rules -> Int
solve1 rules = length $ filter (canContainShinyGoldBag rules) $ Map.keys rules

contentCount :: Rules -> String -> Int
contentCount rules bag =
  let bags = fromJust $ Map.lookup bag rules
      count1 (count, bagName) = count * contentCount rules bagName
  in (sum $ fmap fst bags) + (sum $ fmap count1 bags)

solve2 :: Rules -> Int
solve2 = flip contentCount "shiny gold"

parse :: String -> Rules
parse input = Map.fromList $ fmap parseLine $ lines input where
  parseLine line = (key, values) where
    (key1:key2:_) = take 2 $ words line
    key = key1 <> " " <> key2
    values = fmap parseValue $ filter ((== 4) . length) $ chunksOf 4 $ drop 4 $ words line
    parseValue (num:word1:word2:_) = (read num, word1 <> " " <> word2)

solveIO :: (Rules -> Int) -> IO ()
solveIO solve = do
  input <- parse <$> readFile "input.txt"
  print $ solve input

main = solveIO solve1 >> solveIO solve2
