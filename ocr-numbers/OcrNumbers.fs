module OcrNumbers
(*
// to generate the patterns for matching:
let rows =
    [ "    _  _     _  _  _  _  _  _ "
      "  | _| _||_||_ |_   ||_||_|| |"
      "  ||_  _|  | _||_|  ||_| _||_|"
      "                              " ]

Seq.zip (Seq.zip rows.[0] rows.[1]) (Seq.zip rows.[2] rows.[3])
|> Seq.map (fun ((a, b), (c, d)) -> [ a; b; c; d ] |> System.String.Concat)
|> Seq.chunkBySize 3
|> Seq.map (Seq.reduce (+))
|> Seq.iter (printfn "| \"%s\" ->")
*)

let convert (rows: string list) =
    let isInputValid = List.forall (fun l -> Seq.length l % 3 = 0) rows && List.length rows % 4 = 0
    if not isInputValid then
        None
    else
        Seq.zip (Seq.zip rows.[0] rows.[1]) (Seq.zip rows.[2] rows.[3])
        |> Seq.map (fun ((a, b), (c, d)) -> [ a; b; c; d ] |> System.String.Concat)
        |> Seq.chunkBySize 3
        |> Seq.map (Seq.reduce (+))
        |> Seq.map (fun l ->
            match l with
            | "         || " -> "1"
            | "  | ___  |  " -> "2"
            | "    ___  || " -> "3"
            | " |   _   || " -> "4"
            | " |  ___   | " -> "5"
            | " || ___   | " -> "6"
            | "    _    || " -> "7"
            | " || ___  || " -> "8"
            | " |  ___  || " -> "9"
            | " || _ _  || " -> "0"
            | _ -> "?")
        |> System.String.Concat
        |> Some
