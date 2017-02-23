import Data.Char (toLower)

l2i :: Char -> Int
l2i c = fromEnum c - 96

i2l :: Int -> Char
i2l i = toEnum (i+96) :: Char

data Crypt = Encrypt | Decrypt
  deriving (Eq)

crypt' :: String -> String -> Bool -> String
crypt' [] _ _ = []
crypt' _ [] _ = error "no key!"
crypt' (m:ms) (k:ks) b = enc m k : crypt' ms ks b
  where
    enc :: Char -> Char -> Char
    enc x y | b = i2l $ (l2i x + l2i y - 1) `mod` 26
            | otherwise = i2l $ (l2i x - l2i y + 1) `mod` 26

crypt :: String -> String -> Crypt -> String
crypt msg key encdec
  | kl < ml
    = crypt' lMsg (concat $ replicate ((ml `div` kl) + 1) lKey) b
  | otherwise = crypt' lMsg lKey b
  where
    kl = length key
    ml = length msg
    lMsg = map toLower msg
    lKey = map toLower key
    b | encdec == Encrypt = True
      | otherwise = False

encrypt :: String -> String -> String
encrypt msg key = crypt msg key Encrypt

decrypt :: String -> String -> String
decrypt msg key = crypt msg key Decrypt
