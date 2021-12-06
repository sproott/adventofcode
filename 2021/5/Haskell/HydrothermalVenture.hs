{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Function (on)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype X = X Int deriving (Eq, Ord, Show, Num, Enum)
newtype Y = Y Int deriving (Eq, Ord, Show, Num, Enum)
type Point = (X, Y)
type Line = (Point, Point)
newtype ExpandedLine = ExpandedLine { unExpandedLine :: (Map (X, Y) Int) } deriving (Eq, Show)

instance Semigroup ExpandedLine where
  (ExpandedLine m1) <> (ExpandedLine m2) = ExpandedLine $ Map.unionWith (+) m1 m2

instance Monoid ExpandedLine where
  mempty = ExpandedLine mempty
  mconcat = ExpandedLine . Map.unionsWith (+) . fmap unExpandedLine

expandLine :: Line -> ExpandedLine
expandLine ((x1, y1), (x2, y2)) = ExpandedLine $ Map.fromList $ zip (zip (produceList x1 x2) (produceList y1 y2)) $ repeat 1 where
  produceList :: (Enum a, Ord a) => a -> a -> [a]
  produceList m n = case compare m n of
    GT -> [m, pred m .. n]
    LT -> [m .. n]
    EQ -> repeat m

notDiagonal :: Line -> Bool
notDiagonal ((x1, y1), (x2, y2)) 
  | x1 == x2 = True
  | y1 == y2 = True
  | otherwise = False

solve :: (Line -> Bool) -> [Line] -> Int
solve condition lines = Map.size $ Map.filter (>1) $ unExpandedLine $ mconcat $ fmap expandLine $ filter condition lines

solve1 :: [Line] -> Int
solve1 = solve notDiagonal

solve2 :: [Line] -> Int
solve2 = solve (const True)

parseLine :: String -> Line
parseLine line = ((X x1, Y y1), (X x2, Y y2)) where
  (xy1:_:xy2:_) = words line
  (x1:y1:_) = fmap read $ splitOn "," xy1
  (x2:y2:_) = fmap read $ splitOn "," xy2

parse :: String -> [Line]
parse = fmap parseLine . lines

solveIO :: ([Line] -> Int) -> IO ()
solveIO solve = do
  input <- parse <$> readFile "input.txt"
  print $ solve input

main :: IO ()
main = solveIO solve1 >> solveIO solve2
