solve :: Int -> [Int] -> Int
solve n = product . head . filter ((== 2020) . sum) . groups n

groups :: Int -> [Int] -> [[Int]]
groups _ [] = []
groups 0 _ = []
groups 1 xs = fmap pure xs
groups n (x:xs) = fmap (x:) (groups (n - 1) xs) ++ groups n xs

solveIO :: Int -> IO ()
solveIO n = do
  result <- solve n . fmap read . words <$> readFile "input.txt"
  print result
