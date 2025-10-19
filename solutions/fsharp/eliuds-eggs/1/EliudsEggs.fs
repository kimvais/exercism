module EliudsEggs

open System

let eggCount (n: int) =
    Convert.ToString(n, 2)
    |> Seq.groupBy id
    |> Map.ofSeq
    |> Map.tryFind '1'
    |> function
    | None -> 0
    | Some n -> Seq.length n
