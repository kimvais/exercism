module BeerSong

open System

let numToBottles n =
    match n with
    | 0 -> "No more bottles"
    | 1 -> "1 bottle"
    | x -> sprintf "%d bottles" x

let verse1 n =
    let bottles = numToBottles n
    sprintf "%s of beer on the wall, %s of beer." bottles bottles

let verse2 n =
    match n with
    | 0 -> sprintf "Take it down and pass it around, %s of beer on the wall." (numToBottles n)
    | -1 -> sprintf "Go to the store and buy some more, 99 bottles of beer on the wall."
    | _ -> sprintf "Take one down and pass it around, %s of beer on the wall." (numToBottles n)

let verse (startBottles: int) =
    [|(verse1 startBottles);
      (verse2 (startBottles - 1)) |]

let recite (startBottles: int) (takeDown: int) =
    let endBottles = startBottles-takeDown+1
    let recite' = seq {
        for n in [startBottles..(-1)..(endBottles)] do
            yield! verse n
            if n > endBottles then yield ""
        }
    recite' |> Seq.toList