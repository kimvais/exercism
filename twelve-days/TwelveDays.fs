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
      "two Turtle Doves, "
      "and a Partridge in a Pear Tree." ]

let cardinals =
    [ "second"; "third"; "fourth"; "fifth"; "sixth"; "seventh"; "eighth"; "ninth"; "tenth"; "eleventh"; "twelfth" ]

let getVerse n =
    match n with
    | 1 -> "On the first day of Christmas my true love gave to me: a Partridge in a Pear Tree."
    | _ ->
        sprintf "On the %s day of Christmas my true love gave to me: %s" cardinals.[n - 2]
            (gifts
             |> Seq.skip (12 - n)
             |> System.String.Concat)

let recite start stop =
    [ start .. stop ] |> List.map getVerse
