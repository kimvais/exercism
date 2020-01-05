module Proverb

let makeVerse (l: string list) =
    sprintf "For want of a %s the %s was lost." l.[0] l.[1]

let makeLastVerse b =
    sprintf "And all for the want of a %s." b

let recite (input: string list): string list =
    match List.length input with
    | 0 -> []
    | _ ->
        let first = List.head input
        (input |> List.windowed 2 |> List.map makeVerse) @ [ makeLastVerse first ]
