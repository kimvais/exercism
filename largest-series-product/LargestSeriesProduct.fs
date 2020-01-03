module LargestSeriesProduct

open System

let largestProduct (input: string) span =
    match (String.length input, span) with
    | _ when input |> String.exists (Char.IsDigit >> not) -> None
    | (x, y) when x < y || x < 0 || y < 0 -> None
    | (0, x) when x > 0 -> None
    | (_, 0) -> Some 1
    | _ ->
        input
        |> Seq.map (string >> int)
        |> Seq.windowed span
        |> Seq.map (Seq.reduce (*))
        |> Seq.max
        |> Some
