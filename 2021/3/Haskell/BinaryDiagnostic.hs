import Data.Function (on)
import Data.List (inits, partition, transpose)

type Bit = Bool
type BNum = [Bit]

produceBNum :: (BNum -> Bit) -> [BNum] -> BNum
produceBNum fn = fmap fn . transpose

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

bNumToInt :: BNum -> Int
bNumToInt bNum = sum $ zipWith (*) (boolToInt <$> bNum) $ fmap (2^) [length bNum - 1, length bNum - 2..]

hasMore1s :: BNum -> Bool
hasMore1s bNum = let (ones, zeros) = partition id bNum in length ones > length zeros 

hasMoreOrEq1s :: BNum -> Bool
hasMoreOrEq1s bNum = let (ones, zeros) = partition id bNum in length ones >= length zeros 

matchingBNum :: (BNum -> Bit) -> [BNum] -> BNum
matchingBNum fn bNums = result where
  result = head $ head $ filter ((==1) . length) $ scanned 
  scanned = scanl filterStep bNums [0..bNumLength - 1]
  bNumLength = length $ head $ bNums
  filterStep acc n = filter (\bNum -> bNum !! n == produced) acc where
    produced = fn $ (!! n) $ transpose acc

solve1 :: [BNum] -> Int
solve1 bNums = ((*) `on` bNumToInt) (produceBNum hasMore1s bNums) (produceBNum (not . hasMore1s) bNums)

solve2 :: [BNum] -> Int
solve2 bNums = ((*) `on` bNumToInt) (matchingBNum hasMoreOrEq1s bNums) (matchingBNum (not . hasMoreOrEq1s) bNums)

parseLine :: String -> BNum
parseLine = fmap parseBit where
  parseBit '1' = True
  parseBit '0' = False

parse :: String -> [BNum]
parse = fmap parseLine . lines

solveIO :: ([BNum] -> Int) -> IO ()
solveIO solve = do
  input <- parse <$> readFile "input.txt"
  print $ solve input

main :: IO ()
main = solveIO solve1 >> solveIO solve2
