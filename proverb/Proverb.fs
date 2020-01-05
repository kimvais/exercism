module Proverb

let makeVerse (a, b) =
    sprintf "For want of a %s the %s was lost." a b
    
let makeLastVerse =
    sprintf "And all for the want of a %s."

let recite (input: string list): string list =
    match input with
    | [] -> []
    | _ ->
        let first = List.head input
        (input |> List.pairwise |> List.map makeVerse) @ [ makeLastVerse first ]
