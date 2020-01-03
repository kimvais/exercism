module LargestSeriesProduct

open System

let largestProduct (input: string) span =
    match input with
    | x when x |> String.exists (Char.IsDigit >> not) -> None
    | _ ->

        let l = String.length input
        match (l, span) with
        | (_, 0) -> Some 1
        | (0, x) when x > 0 -> None
        | (x, y) when x < y || x < 0 || y < 0 -> None
        | _ ->
            input
            |> Seq.map (string >> int)
            |> Seq.windowed span
            |> Seq.map (Seq.reduce (*))
            |> Seq.max
            |> Some
