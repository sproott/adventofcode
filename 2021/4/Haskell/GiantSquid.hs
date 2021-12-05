import Data.List (scanl', transpose)

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

runBoards :: [Int] -> [Board] -> (Int, Board)
runBoards ns boards = let
  gameSequence :: [(Int, [Board])]
  gameSequence = scanl' (\(_, boards) n -> (n, fmap (markNum n) boards)) (-1, boards) ns
  lastRound :: (Int, [Board])
  lastRound = head $ dropWhile (not . any isBoardWinning . snd) gameSequence
  in (fst lastRound, head $ filter isBoardWinning $ snd lastRound)
