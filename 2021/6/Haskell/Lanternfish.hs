{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

import Data.List (iterate')
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Fishes = Map Int Int

runFish :: Int -> Fishes -> Fishes
runFish n = (!! n) . iterate' go where
  go :: Fishes -> Fishes
  go fish = 
      Map.insert 8 (Map.findWithDefault 0 0 fish) 
    $ Map.insertWith (+) 6 (Map.findWithDefault 0 0 fish) 
    $ Map.mapKeys (subtract 1) 
    $ Map.delete 0 fish

solve1 :: Fishes -> Int
solve1 = sum . fmap snd . Map.toList . runFish 80

solve2 :: Fishes -> Int
solve2 = sum . fmap snd . Map.toList . runFish 256

parse :: String -> Fishes
parse = foldr (uncurry (Map.insertWith (+)) . \timer -> (timer, 1)) Map.empty . fmap read . splitOn ","

solveIO :: (Fishes -> Int) -> IO ()
solveIO solve = do
  input <- parse <$> readFile "input.txt"
  print $ solve input

main :: IO ()
main = solveIO solve1 >> solveIO solve2
