import Data.List (delete, sortOn)

scrabble :: String -> String -> Bool
scrabble _ []      = True
scrabble ys (x : xs)
  | x `elem` ys = scrabble (delete x ys) xs
  | '?' `elem` ys = scrabble (delete '?' ys) xs
  | otherwise = False

letterScore :: Char -> Int
letterScore x | x == '?'              = 0
              | x `elem` "eaionrtlsu" = 1
              | x `elem` "dg"         = 2
              | x `elem` "bcmp"       = 3
              | x `elem` "fhvwy"      = 4
              | x == 'k'              = 5
              | x `elem` "jx"         = 8
              | otherwise             = 10

longest :: String -> IO String
longest xs = last . filter (scrabble xs) <$> readWords

deleteReturn :: String -> String
deleteReturn xs = take (length xs - 1) xs

readWords :: IO [String]
readWords = (sortOn length . map deleteReturn) . lines <$> readFile "enable1.txt"
