import Data.List (foldl')

data Position = Position { horizontal :: Int, vertical :: Int, aim :: Int }
data Direction = Forward | Up | Down
type Move = (Direction, Int)

initPosition :: Position
initPosition = Position 0 0 0

stepPosition1 :: Move -> Position -> Position
stepPosition1 (Forward, n) pos = pos { horizontal = horizontal pos + n }
stepPosition1 (Down, n) pos = pos { vertical = vertical pos + n }
stepPosition1 (Up, n) pos = pos { vertical = vertical pos - n }

stepPosition2 :: Move -> Position -> Position
stepPosition2 (Forward, n) pos = pos { horizontal = horizontal pos + n, vertical = vertical pos + aim pos * n }
stepPosition2 (Down, n) pos = pos { aim = aim pos + n }
stepPosition2 (Up, n) pos = pos { aim = aim pos - n }

runSubmarine :: (Move -> Position -> Position) -> [Move] -> Position
runSubmarine step = foldl' (flip step) initPosition

solve :: (Move -> Position -> Position) -> [Move] -> Int
solve step = (\(Position h v _) -> h * v) . runSubmarine step

solve1 :: [Move] -> Int
solve1 = solve stepPosition1

solve2 :: [Move] -> Int
solve2 = solve stepPosition2

parse :: String -> [Move]
parse = fmap (parseMove . words) . lines where
  parseMove ("forward":n:_) = (Forward, read n)
  parseMove ("up":n:_) = (Up, read n)
  parseMove ("down":n:_) = (Down, read n)

solveIO :: ([Move] -> Int) -> IO ()
solveIO solve = do
  input <- parse <$> readFile "input.txt"
  print $ solve input

main :: IO ()
main = solveIO solve1 >> solveIO solve2
