module Main (main) where
import Data.Char

capitalise :: String -> String
capitalise (x:xs) = toUpper x:xs

data State = Start | Exit | Error | S Int
data Wire = White | Red | Green | Orange | Black deriving (Read)

next :: State -> Wire -> State
next Start White  = S 1
next Start Red    = S 2
next (S 1) White  = S 2
next (S 1) Orange = S 3
next (S 2) Red    = Start
next (S 2) Black  = S 3
next (S 3) Black  = S 3
next (S 3) Orange = S 4
next (S 3) Green  = S 5
next (S 4) Green  = Exit
next (S 5) Orange = Exit
next _     _      = Error

out :: State -> String
out Exit = "Defused"
out _    = "Boom"

main :: IO ()
main = interact $ out . foldl next Start . map (read . capitalise) . lines
