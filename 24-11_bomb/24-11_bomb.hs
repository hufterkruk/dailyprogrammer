module Main where
  import Data.Maybe
  import Data.Char
  import Data.List
  import Data.List.Split

  --main = interact $ handleWires S0 . readWires
  main = bonus

  data State = S0 | S1 | S2 | S3 | S4 | S5 | E | Boom
    deriving (Eq)

  data Wire = White | Black | Red | Orange | Green
    deriving (Read, Eq)

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
  handleWires E []     = "defused\n"
  handleWires _ []     = "boom\n"
  handleWires E _      = "boom\n"
  handleWires s (x:xs) =
    if transition /= Boom
      then handleWires transition xs
      else "boom\n"
    where transition = stateTransition s x

  bonus = interact $ doPerms . bonusRead

  bonusRead :: String -> [Wire]
  bonusRead = map (read . capitalize) . concat . map f . map (splitOn " ") . lines
    where f ["black",_] = ["black"]
          f [x,y] = replicate (read y) x
          f _ = []
          capitalize = \(x:xs) -> toUpper x : xs

  doPerms :: [Wire] -> String
  doPerms xs =
    if (any (==Green) xs) && (any (==Orange) xs) -- Must have at least one green and one orange
      then
        if any (=="defused\n") perms
          then "defusable\n"
          else "not defusable\n"
      else "not defusable\n"
    where perms = map (handleWires S0) (permutations xs)
