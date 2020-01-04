module Hamming

let distance (strand1: string) (strand2: string): int option =
    if String.length strand1 = String.length strand2 then
        Seq.zip strand1 strand2
        |> Seq.sumBy (fun (a, b) ->
            match a <> b with
            | true -> 1
            | false -> 0)
        |> Some
    else None
