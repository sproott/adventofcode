import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split (splitWhen)

type Answers = [Char]
type Group = [Answers]

solve1 :: [Group] -> Int
solve1 groups = sum $ Set.size . toSet <$> groups where
  toSet group = foldr Set.union Set.empty $ Set.fromList <$> group

solve2 :: [Group] -> Int
solve2 groups = sum $ Set.size . toSet <$> groups where
  toSet group = foldr1 Set.intersection $ Set.fromList <$> group

parse :: String -> [Group]
parse = splitWhen null . lines

solveIO :: ([Group] -> Int) -> IO ()
solveIO solve = do
  contents <- readFile "input.txt"
  print $ solve $ parse contents

main :: IO ()
main = solveIO solve1 >> solveIO solve2
