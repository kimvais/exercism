module BeerSong

open System

let numToBottles n =
    match n with
    | 0 -> "no more bottles"
    | 1 -> "1 bottle"
    | x -> sprintf "%d bottles" x

// If it's stupid but it works...
let upperBottles s =
    match s with
    | s when s = "no more bottles" -> "No more bottles"
    | _ -> s
    
let verse1 n =
    let bottles = numToBottles n
    let upperbottles = upperBottles bottles
    sprintf "%s of beer on the wall, %s of beer." upperbottles bottles

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