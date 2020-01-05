module SecretHandshake

let operations = [ "wink"; "double blink"; "close your eyes"; "jump" ]

let commands number =
    let shake =
        operations
        |> List.indexed
        |> List.filter (fun (i, x) -> (pown 2 i) &&& number > 0)
        |> List.map (fun (_, x) -> x)
    match number > 15 with
    | false -> shake
    | true -> shake |> List.rev
