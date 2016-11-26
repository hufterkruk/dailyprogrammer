-- If you cut a white cable you can't cut white or black cable.
-- If you cut a red cable you have to cut a green one
-- If you cut a black cable it is not allowed to cut a white, green or orange one
-- If you cut a orange cable you should cut a red or black one
-- If you cut a green one you have to cut a orange or white one
-- If you cut a purple cable you can't cut a purple, green, orange or white cable

    module Main where
      main = interact (cutWire . lines)

      cutWire :: [String] -> String
      cutWire [] = "Bomb defused\n"
      cutWire [x] = "Bomb defused\n"
      cutWire (x:y:xs) =
        if wireIsSafe x y
          then cutWire (y:xs)
        else "Boom\n"

      wireIsSafe :: String -> String -> Bool
      wireIsSafe "white"  "white"  = False
      wireIsSafe "white"  "black"  = False
      wireIsSafe "white"  _        = True
      wireIsSafe "black"  "white"  = False
      wireIsSafe "black"  "green"  = False
      wireIsSafe "black"  "orange" = False
      wireIsSafe "black"  _        = True
      wireIsSafe "purple" "black"  = True
      wireIsSafe "purple" "red"    = True
      wireIsSafe "purple" _        = False
      wireIsSafe "red"    "green"  = True
      wireIsSafe "red"    _        = False
      wireIsSafe "green"  "orange" = True
      wireIsSafe "green"  "white"  = True
      wireIsSafe "green"  _        = True
      wireIsSafe "orange" "red"    = True
      wireIsSafe "orange" "black"  = True
      wireIsSafe "orange" _        = False
