import Data.List (delete, sortOn)

scrabble :: String -> String -> Bool
scrabble _ []      = True
scrabble ys (x : xs)
  | x `elem` ys = scrabble (delete x ys) xs
  | '?' `elem` ys = scrabble (delete '?' ys) xs
  | otherwise = False

letterScore :: Char -> Int
letterScore x | x `elem` "eaionrtlsu" = 1
              | x `elem` "dg"         = 2
              | x `elem` "bcmp"       = 3
              | x `elem` "fhvwy"      = 4
              | x == 'k'              = 5
              | x `elem` "jx"         = 8
              | otherwise             = 10

wordScore :: String -> String -> Int
wordScore ys [] = -1 * (sum . map letterScore) ys
wordScore ys (x:xs)
  | not (scrabble ys (x:xs)) = 0
  | x `elem` ys = letterScore x + wordScore (delete x ys) xs
  | '?' `elem` ys = wordScore (delete x ys) xs
  | otherwise = undefined

highest :: String -> IO String
highest xs = last . sortOn (wordScore xs) <$> readWords xs

longest :: String -> IO String
longest xs = last . sortOn length <$> readWords xs

readWords :: String -> IO [String]
readWords xs = filter (scrabble xs) . map (delete '\r') . lines <$> readFile "enable1.txt"
