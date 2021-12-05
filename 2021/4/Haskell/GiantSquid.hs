import Data.Function (on)
import Data.List (scanl', transpose)
import Data.List.Split (splitOn, splitWhen)

data CellType = Marked | Unmarked deriving (Eq, Show)
type Cell = (Int, CellType)
type Board = [[Cell]]

markNum :: Int -> Board -> Board
markNum num board = fmap (\(n, t) -> if n == num then (n, Marked) else (n, t)) <$> board

isBoardWinning :: Board -> Bool
isBoardWinning board = any isWinning $ board <> transpose board where
  isWinning = all ((== Marked) . snd)

parseBoard :: [String] -> Board
parseBoard = fmap $ fmap (mkCell . read) . words where
  mkCell n = (n, Unmarked)

parse :: String -> ([Int], [Board])
parse input = (ns, boards) where
  (nstr:_:rest) = lines input
  ns = fmap read $ splitOn "," nstr
  boards = parseBoard <$> splitWhen null rest

runBoards :: [Int] -> [Board] -> ([Board] -> Bool) -> (Int, Board)
runBoards ns boards endingCondition = let
  gameSequence :: [(Int, [Board])]
  gameSequence = scanl' (\(_, boards) n -> (n, fmap (markNum n) boards)) (-1, boards) ns
  (gameRounds, (lastRound:_)) = span (not . endingCondition . snd) gameSequence
  secondLastRound = last gameRounds
  winningBoard = snd $ head $ filter (\(b1, b2) -> not (isBoardWinning b1) && isBoardWinning b2) $ (zip `on` snd) secondLastRound lastRound
  in (fst lastRound, winningBoard)

solve :: ([Board] -> Bool) -> [Int] -> [Board] -> Int
solve endingCondition ns boards = 
  let (n, board) = runBoards ns boards endingCondition
  in  n * (sum $ fmap fst $ filter ((== Unmarked) . snd) $ concat board)

solve1 :: [Int] -> [Board] -> Int
solve1 = solve $ any isBoardWinning

solve2 :: [Int] -> [Board] -> Int
solve2 = solve $ all isBoardWinning

solveIO :: ([Int] -> [Board] -> Int) -> IO ()
solveIO solve = do
  input <- parse <$> readFile "input.txt"
  print $ uncurry solve $ input

main :: IO ()
main = solveIO solve1 >> solveIO solve2
