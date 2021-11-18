{-# LANGUAGE TypeApplications #-}

import Data.Char (isDigit)
import Data.List (intercalate, span)
import Data.Maybe (isJust)
import Text.Read

type Field = Maybe Bool

data Unit = Cm | In deriving (Eq, Show)

readUnit :: String -> Maybe Unit
readUnit "cm" = pure Cm
readUnit "in" = pure In
readUnit _    = Nothing

data Record = Record { byr :: Field, iyr :: Field, eyr :: Field, hgt :: Field, hcl :: Field, ecl :: Field, pid :: Field } deriving (Eq, Show)

emptyRecord :: Record
emptyRecord = Record Nothing Nothing Nothing Nothing Nothing Nothing Nothing

maybeToField :: Maybe a -> Field
maybeToField Nothing = pure False
maybeToField (Just _) = pure True

boolToMaybe :: Bool -> Maybe ()
boolToMaybe True = pure ()
boolToMaybe False = Nothing

validateHgt :: String -> Bool
validateHgt str = isJust $ do
  let (numberStr, unitStr) = span isDigit str
  number <- readMaybe numberStr
  unit <- readUnit unitStr
  let validate num Cm = num >= 150 && num <= 193
      validate num In = num >= 50 && num <= 76
  boolToMaybe $ validate number unit

validateHcl :: String -> Bool
validateHcl ('#':num) = length num == 6 && all (flip elem (['0'..'9'] ++ ['a'..'f'])) num
validateHcl _ = False

validateEcl :: String -> Bool
validateEcl = flip elem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validatePid :: String -> Bool
validatePid str = length str == 9 && isJust (readMaybe @Int str)

validateNum :: (Int, Int) -> String -> Bool
validateNum (min, max) str = isJust $ do 
  num <- readMaybe str
  boolToMaybe $ num >= min && num <= max

updateRecord :: Record -> String -> String -> Record
updateRecord r "byr" val = r { byr = pure $ validateNum (1920, 2002) val }
updateRecord r "iyr" val = r { iyr = pure $ validateNum (2010, 2020) val }
updateRecord r "eyr" val = r { eyr = pure $ validateNum (2020, 2030) val }
updateRecord r "hgt" val = r { hgt = pure $ validateHgt val }
updateRecord r "hcl" val = r { hcl = pure $ validateHcl val }
updateRecord r "ecl" val = r { ecl = pure $ validateEcl val }
updateRecord r "pid" val = r { pid = pure $ validatePid val }
updateRecord r _     _   = r

recordValid :: (Field -> Bool) -> Record -> Bool
recordValid validate r = and $ fmap (validate . ($ r)) [byr, iyr, eyr, hgt, hcl, ecl, pid]

solve :: (Field -> Bool) -> [Record] -> Int
solve fn = length . filter (recordValid fn)

valid1 :: Field -> Bool
valid1 = isJust

valid2 :: Field -> Bool
valid2 = (==) $ Just True

readRecords :: [String] -> [Record]
readRecords lines = fmap parse lines where
  parse line = mkRecord $ fmap (mkTuple . splitOn ':') $ words line
  mkTuple (fst:snd:_) = (fst, snd)
  mkRecord :: [(String, String)] -> Record
  mkRecord kvs = go kvs emptyRecord
  go [] record = record
  go ((k, v):kvs) record = go kvs $ updateRecord record k v

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn x xs = first : splitOn x (safeTail rest) where
  (first, rest) = span (/= x) xs
  safeTail [] = []
  safeTail (x:xs) = xs

solveIO :: (Field -> Bool) -> IO ()
solveIO fn = do
  lines <- fmap (intercalate " ") . splitOn "" . lines <$> readFile "input.txt"
  let records = readRecords lines
  print $ length $ filter (recordValid fn) records

main1 :: IO ()
main1 = solveIO valid1

main2 :: IO ()
main2 = solveIO valid2
