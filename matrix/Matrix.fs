module Matrix

let split (c: char) (s: string) = s.Split(c)

let parseStringToMatrix =
    split '\n'
    >> Array.map
        (split ' '
         >> Array.map int
         >> Array.toList)
    >> Array.toList

let row index m =
    (parseStringToMatrix m).[index - 1]

let column index matrix =
    parseStringToMatrix matrix |> List.map (fun row -> row.[index - 1])
