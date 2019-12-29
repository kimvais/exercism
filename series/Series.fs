module Series

let slices (str: string) length =
    match length with
    | l when l > 0 ->
        match str
              |> Seq.windowed length
              |> (Seq.map System.String.Concat)
              |> Seq.toList with
        | s when s.IsEmpty -> None
        | s -> Some s
    | _ -> None
