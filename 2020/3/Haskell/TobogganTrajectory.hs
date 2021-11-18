data Square = Empty | Tree deriving (Eq, Show)

readSquare :: Char -> Square
readSquare '.' = Empty
readSquare '#' = Tree

type Map = [[Square]]

loadMap :: [String] -> Map
loadMap lines = fmap (cycle . fmap readSquare) lines

solve :: (Int, Int) -> Map -> Int
solve (down, right) xs = length $ filter (== Tree) $ zipWith (!!) (sieve down xs) [right, 2 * right ..]

sieve :: Int -> [a] -> [a]
sieve n xs = go n xs where
  go _ [] = []
  go 0 (x:xs) = x : go (n - 1) xs
  go k (_:xs) = go (k - 1) xs

solve1 :: Map -> Int
solve1 = solve (1, 3) 

solve2 :: Map -> Int
solve2 xs = product $ fmap (flip solve xs) [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)] 

solveIO :: (Map -> Int) -> IO ()
solveIO solve = do
  map <- loadMap . lines <$> readFile "input.txt"
  print $ solve map

main1 :: IO ()
main1 = solveIO solve1

main2 :: IO ()
main2 = solveIO solve2
