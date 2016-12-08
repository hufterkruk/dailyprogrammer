import Data.List (delete, sortOn)

scrabble :: String -> String -> Bool
scrabble _ []      = True
scrabble ys (x : xs)
  | x `elem` ys = scrabble (delete x ys) xs
  | '?' `elem` ys = scrabble (delete '?' ys) xs
  | otherwise = False
  
longest :: String -> IO String
longest xs = last . filter (scrabble xs) <$> readWords

readWords :: IO [String]
readWords = (sortOn length . map (delete '\r')) . lines <$> readFile "enable1.txt"
