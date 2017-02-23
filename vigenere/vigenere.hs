import Data.Char (toLower)

encrypt :: String -> String -> String
ecrypt [] _ = []
encrypt _ [] = error "doe ff nie"
encrypt [m] [k] = toEnum (fromEnum toLower m + fromEnum toLower k)
encrypt msg@(m:ms) key@(k:ks)
  | length key < length msg
    = encrypt msg (concat $ replicate ((ml `div` kl) + 1) key)
  | otherwise
    = toEnum (fromEnum m' + fromEnum k') : encrypt ms ks
  where
    kl = length key
    ml = length msg
    m' = toLower m
    k' = toLower k
