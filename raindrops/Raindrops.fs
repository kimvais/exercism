module Raindrops

let getSound number =
    match number with
    | 3 -> "Pling"
    | 5 -> "Plang"
    | 7 -> "Plong"
    | _ -> ""

let checkIfSome option x =
    match x with
    | None -> false
    | Some _ -> true

let getSounds number =
    seq {
        for divisor in [| 3; 5; 7 |] do
            if number % divisor = 0 then yield getSound divisor
    }

let convert (number: int): string =
    let sound = getSounds number |> String.concat ""
    if sound = "" then
        sprintf "%d" number
    else
        sound
