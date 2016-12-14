swapLetter :: String -> String -> String -> Int -> [String]
swapLetter [] _ _ _ = []
swapLetter (x:xs) (y:ys) orig index
  | x == y = swapLetter xs ys orig (index+1)
  | otherwise = [(take index orig ++ (y:xs))] ++ swapLetter xs ys orig (index+1)

swapLetters :: String -> String -> IO ()
swapLetters xs ys = putStrLn $ unlines $ xs : swapLetter xs ys ys 0
