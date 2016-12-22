days :: [String]
days = ["twelfth",
        "eleventh",
        "tenth",
        "ninth",
        "eighth",
        "seventh",
        "sixth",
        "fifth",
        "fourth",
        "third",
        "second",
        "first"]

gifts :: [String]
gifts = ["Drummers Drumming",
         "Pipers Piping",
         "Lords a leaping",
         "Ladies Dancing",
         "Maids a milking",
         "Swans a swimming",
         "Geese a Laying",
         "Golden Rings",
         "Calling Birds",
         "French Hens",
         "Turtle Doves",
         "Partridge in a Pear Tree"]

wrapGifts :: [String] -> Int -> String
wrapGifts [] _ = ""
wrapGifts _  0 = ""
wrapGifts (g:gs) i
  = show i ++ " " ++ g ++ "\n" ++ wrapGifts gs (i-1)

twelveDays :: [String] -> [String] -> Int -> String
twelveDays _ _ 0 = ""
twelveDays (d:ds) (g:gs) i
  = twelveDays ds gs (i-1) ++
    "On the " ++ d ++ " day of Christmas my true love sent to me:\n" ++
    wrapGifts (g:gs) i ++ "\n"
