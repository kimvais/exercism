module TwelveDays

let gifts =
    [ "twelve Drummers Drumming, "
      "eleven Pipers Piping, "
      "ten Lords-a-Leaping, "
      "nine Ladies Dancing, "
      "eight Maids-a-Milking, "
      "seven Swans-a-Swimming, "
      "six Geese-a-Laying, "
      "five Gold Rings, "
      "four Calling Birds, "
      "three French Hens, "
      "two Turtle Doves, and "
      "a Partridge in a Pear Tree." ]

let cardinals =
    [ "first"; "second"; "third"; "fourth"; "fifth"; "sixth"; "seventh"; "eighth"; "ninth"; "tenth"; "eleventh"; "twelfth" ]

let getVerse n =
    sprintf "On the %s day of Christmas my true love gave to me: %s" cardinals.[n - 1]
        (gifts.[12 - n..] |> List.reduce (+))

let recite start stop =
    [ start .. stop ] |> List.map getVerse
