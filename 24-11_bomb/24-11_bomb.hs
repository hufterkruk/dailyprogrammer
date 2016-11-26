module Main where
  import Data.Maybe
  import Data.Char

  main = interact $ handleWires S0 . readWires

  data State = S0 | S1 | S2 | S3 | S4 | S5 | E | Boom
    deriving (Eq)

  data Wire = White | Black | Red | Orange | Green
    deriving (Read)

  readWires :: String -> [Wire]
  readWires = map (read . capitalize) . lines
   where capitalize = \(x:xs) -> toUpper x : xs

  stateTransition :: State -> Wire -> State
  stateTransition S0 White  = S1
  stateTransition S0 Red    = S2
  stateTransition S1 White  = S2
  stateTransition S1 Orange = S3
  stateTransition S2 Red    = S0
  stateTransition S2 Black  = S3
  stateTransition S3 Black  = S3
  stateTransition S3 Orange = S4
  stateTransition S3 Green  = S5
  stateTransition S4 Green  = E
  stateTransition S5 Orange = E
  stateTransition _  _      = Boom

  handleWires :: State -> [Wire] -> String
  handleWires _ []     = "defused"
  handleWires E _      = "defused"
  handleWires s (x:xs) =
    if transition /= Boom
      then handleWires transition xs
      else "boom"
    where transition = stateTransition s x

  bonus = undefined
  
