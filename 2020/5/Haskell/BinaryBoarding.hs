import Data.List (sort)

bitsToNum :: [Bool] -> Int
bitsToNum xs = sum $ zipWith f xs [length xs - 1, length xs - 2 .. 0] where
  f x y = (if x then 1 else 0) * (2 ^ y)

readFB :: Char -> Bool
readFB 'F' = False
readFB 'B' = True

readLR :: Char -> Bool
readLR 'L' = False
readLR 'R' = True

solve1 :: String -> Int
solve1 str = row * 8 + column where
  (fb, lr) = span (flip elem "FB") str
  row = bitsToNum $ fmap readFB fb
  column = bitsToNum $ fmap readLR lr

main1 :: IO ()
main1 = do
  lines <- lines <$> readFile "input.txt"
  print $ maximum $ fmap solve1 lines

main2 :: IO ()
main2 = do
  lines <- lines <$> readFile "input.txt"
  let seats = sort $ fmap solve1 lines
      allSeats = [minimum seats .. maximum seats]
  print $ head $ filter (not . flip elem seats) allSeats
