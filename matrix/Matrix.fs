module Matrix

let make (s: string) =
    s.Split('\n')
    |> Array.map (fun s ->
        (s.Split(' ')
         |> Array.map int
         |> Array.toList))
    |> Array.toList

let row index matrix =
    (make matrix).[index - 1]

let column index matrix =
    make matrix |> List.map (fun row -> row.[index - 1])
