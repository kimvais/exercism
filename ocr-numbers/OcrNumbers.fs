module OcrNumbers

(*
A character has 7 segments, two pipes at sides and three underscores in the middle
Corners must always be spaces. If we number them by positions as follows:
012
345
678
9AB
Positions 0,2,9,A and B must always be ' ', -> E
Positions 1,4 and 7 can either be set '_' or not set ' ' -> U
Position 3,5,6 and 8 can be set '|' or not set ' ' -> P

so:
EUE
PUP
PUP
EEE

concatenated:
EUEPUPPUPEEE
*)

type ValidDefinition =
    | E
    | U
    | P

let validCharacters = [ E; U; E; P; U; P; P; U; P; E; E; E ]

let validateOne validForPos c =
    match validForPos with
    | E -> c = ' '
    | U -> c = '_' || c = ' '
    | P -> c = '|' || c = ' '


let convert (input: string list) =
    match List.length input with
    | 4 ->
        let character = input
        match Seq.forall2 validateOne validCharacters (character |> List.reduce (+)) with
        | false -> None
        | true -> Some "0"
    | _ -> None
