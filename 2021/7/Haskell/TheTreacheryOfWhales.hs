import Data.List.Split (splitOn)

parse :: String -> [Int]
parse = fmap read . splitOn ","

solve :: (Int -> Int -> Int) -> [Int] -> Int
solve f xs = minimum $ [sum $ fmap (f target) xs | target <- [minimum xs .. maximum xs]]

solve1 :: [Int] -> Int
solve1 = solve $ \target -> abs . subtract target

solve2 :: [Int] -> Int
solve2 = solve $ \target -> (\n -> (n + 1) * n `div` 2) . abs . subtract target

solveIO :: ([Int] -> Int) -> IO ()
solveIO solve = do
  input <- parse <$> readFile "input.txt"
  print $ solve input

main :: IO ()
main = solveIO solve1 >> solveIO solve2
