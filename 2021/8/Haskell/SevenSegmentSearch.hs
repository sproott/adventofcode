import Control.Arrow (first)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

classifyNum :: Int -> [Int] -> Int
classifyNum n = case n of
  1 -> filterNums 2
  4 -> filterNums 4
  7 -> filterNums 3
  8 -> filterNums 7
  where filterNums n = length . filter (== n)

classifyGroup :: [Int] -> Int
classifyGroup = length . filter (`elem` [2, 3, 4, 7])

parseLine :: String -> ([String], [String])
parseLine line = let 
  [inputs, outputs] = words <$> splitOn " | " line
  in (inputs, outputs)

parse :: String -> [([String], [String])]
parse = fmap parseLine . lines

main :: IO ()
main = do
  input <- parse <$> readFile "input.txt"
  print input
