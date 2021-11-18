{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

import Data.Char (isDigit)
import Data.Function (on)
import Data.Maybe (catMaybes)
import Control.Applicative

data Input = Input (Int, Int) Char String deriving (Eq, Show)

solve1 :: Input -> Bool
solve1 (Input (min, max) c cs) = 
  let count = length $ filter (== c) cs
   in count >= min && count <= max

solve2 :: Input -> Bool
solve2 (Input (i1, i2) c cs) = ((/=) `on` ((== c) . (cs !!) . (subtract 1))) i1 i2

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap fn (Parser p) = Parser $ \str -> do
    (a, rest) <- p str
    pure (fn a, rest) 

instance Applicative Parser where
  pure a = Parser $ \str -> pure (a, str)
  (Parser p1) <*> (Parser p2) = Parser $ \str -> do
    (fn, str') <- p1 str
    (a, str'') <- p2 str'
    pure (fn a, str'')

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \str -> p1 str <|> p2 str

predicateP :: (Char -> Bool) -> Parser Char
predicateP pred = Parser $ \case 
  [] -> Nothing
  (x:xs) -> if pred x then pure (x, xs) else Nothing

charP :: Char -> Parser Char
charP c = predicateP (== c)

charP' :: Parser Char
charP' = Parser $ \case
  [] -> Nothing
  (x:xs) -> pure (x, xs)

digitP :: Parser Char
digitP = predicateP isDigit

numberP :: Parser Int
numberP = read <$> many digitP

lineP :: Parser Input
lineP = do
  min <- numberP
  charP '-'
  max <- numberP
  charP ' '
  c <- charP'
  charP ':'
  charP ' '
  cs <- many charP'
  pure $ Input (min, max) c cs

solveIO :: (Input -> Bool) -> IO ()
solveIO fn = do
  inputs <- fmap fst . catMaybes . fmap (runParser lineP) . lines <$> readFile "input.txt"
  print $ length $ filter id $ fmap fn inputs

main1 :: IO ()
main1 = solveIO solve1

main2 :: IO ()
main2 = solveIO solve2
