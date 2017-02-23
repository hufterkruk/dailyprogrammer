import Data.Char (toLower)

l2i :: Char -> Int
l2i c = fromEnum c - 96

i2l :: Int -> Char
i2l i = toEnum (i+96) :: Char

encrypt' :: String -> String -> String
encrypt' [] _ = []
encrypt' _ [] = error "no key!"
encrypt' (m:ms) (k:ks) = enc m k : encrypt' ms ks
  where
    enc :: Char -> Char -> Char
    enc a b = i2l $ (l2i a + l2i b) `mod` 26

encrypt :: String -> String -> String
encrypt msg key
  | kl < ml
    = encrypt' lMsg (concat $ replicate ((ml `div` kl) + 1) lKey)
  | otherwise = encrypt' lMsg lKey
  where
    kl = length key
    ml = length msg
    lMsg = map toLower msg
    lKey = map toLower key
