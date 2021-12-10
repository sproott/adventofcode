import Data.Function (on)
import Data.List (transpose)
import Data.Maybe (fromMaybe)

type Location = (Int, Bool)
type Tube = [Location]
type Tubes = [Tube]

mkLocation :: Int -> Location
mkLocation n = (n, True)

mapWide :: (Maybe a -> a -> Maybe a -> b) -> [a] -> [b]
mapWide _ [] = []
mapWide f xs@(x:y:_) = f Nothing x (pure y) : go f xs where
  go f (x:[]) = pure $ f Nothing x Nothing
  go f (x:y:[]) = pure $ f (pure x) y Nothing
  go f xs@(x:y:z:_) = f (pure x) y (pure z) : go f (tail xs)
mapWide f xs@(x:_) = pure $ f Nothing x Nothing

removeLocation :: Location -> Location
removeLocation (n, _) = (n, False)

filterRows :: [Tube] -> [Tube]
filterRows = fmap (mapWide transformTube) where
  transformTube prev cur next = if ((&&) `on` (fromMaybe True . fmap ((> (fst cur)) . fst))) prev next then cur else removeLocation cur

filterTubes :: [Tube] -> [Tube]
filterTubes = transpose . filterRows . transpose . filterRows

parseLine :: String -> Tube
parseLine = fmap (mkLocation . read . pure)

parse :: String -> Tubes
parse = fmap parseLine . lines

solve1 :: [Tube] -> Int
solve1 = sum . fmap ((+1) . fst) . filter snd . concat . filterTubes

solveIO :: ([Tube] -> Int) -> IO ()
solveIO solve = do
  input <- parse <$> readFile "input.txt"
  print $ solve input

main :: IO ()
main = solveIO solve1
