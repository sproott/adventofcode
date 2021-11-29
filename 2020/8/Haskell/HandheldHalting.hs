import Debug.Trace
import Data.Maybe (catMaybes, isJust)

data Instruction = Nop Int | Acc Int | Jmp Int deriving (Eq, Show)
data Machine = Machine {is :: [Instruction], ips :: [Int], acc :: Int, ip :: Int } deriving (Eq, Show)

initMachine :: [Instruction] -> Machine
initMachine is = Machine is [] 0 0

stepMachine :: Machine -> Maybe Machine
stepMachine machine = if outOfBounds || repeated then Nothing else pure $ step $ historizeIp machine where 
  step machine =  
    case is machine !! ip machine of
      Nop _ -> machine { ip = ip machine + 1 }
      Acc n -> machine { acc = n + acc machine, ip = ip machine + 1 }
      Jmp n -> machine { ip = ip machine + n }
    where
  historizeIp machine = machine { ips = ips machine ++ [ip machine] }
  outOfBounds = ip machine < 0 || ip machine >= length (is machine) 
  repeated = ip machine `elem` ips machine

runMachine :: Machine -> Machine
runMachine machine = case stepMachine machine of
  Nothing -> machine
  Just machine -> runMachine machine

machineSuccess :: Machine -> Bool
machineSuccess machine = ip machine == length (is machine)

machineVariations :: Machine -> [Machine]
machineVariations machine = catMaybes $ fmap swap [0..length (is machine) - 1] where
  swap idx = case is machine !! idx of
    Nop n -> Just $ modifyMachine idx (Jmp n)
    Jmp n -> Just $ modifyMachine idx (Nop n)
    _ -> Nothing
  modifyMachine n x = machine { is = modify (is machine) n x }

modify :: [a] -> Int -> a -> [a]
modify (x:xs) 0 y = y : xs
modify (x:xs) n y = x : modify xs (n - 1) y

parseInstruction :: String -> Instruction
parseInstruction line = case words line of
  ("nop":num:_) -> Nop $ read' num
  ("acc":num:_) -> Acc $ read' num
  ("jmp":num:_) -> Jmp $ read' num
  where
    read' ('+':num) = read num
    read' num = read num

solve1 :: Machine -> Int
solve1 = acc . runMachine

solve2 :: Machine -> Int
solve2 = acc . head . filter machineSuccess . fmap runMachine . machineVariations

solveIO :: (Machine -> Int) -> IO ()
solveIO solve = do
  input <- initMachine . fmap parseInstruction . lines <$> readFile "input.txt"
  print $ solve input

main = solveIO solve1 >> solveIO solve2
