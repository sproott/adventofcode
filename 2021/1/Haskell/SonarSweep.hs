mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent _ [] = []
mapAdjacent f [x] = []
mapAdjacent f (x:y:rest) = f x y : mapAdjacent f (y:rest)

genWindowSums :: Int -> [Int] -> [Int]
genWindowSums n xs 
  | length xs < n = []
  | otherwise = sum (take n xs) : genWindowSums n (tail xs)

solve1 :: [Int] -> Int
solve1 = length . filter (> 0) . mapAdjacent subtract

solve2 :: [Int] -> Int
solve2 = solve1 . genWindowSums 3

parse :: String -> [Int]
parse = fmap read . lines

solveIO :: ([Int] -> Int) -> IO ()
solveIO solve = do 
  input <- parse <$> readFile "input.txt"
  print $ solve input

main :: IO ()
main = solveIO solve1 >> solveIO solve2
