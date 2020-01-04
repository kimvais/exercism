module KindergartenGarden

type Plant = | Clover | Grass | Radishes | Violets 

let getOffset student =
    let students =
        [ "Alice"; "Bob"; "Charlie"; "David"; "Eve"; "Fred"; "Ginny"; "Harriet"; "Ileana"; "Joseph"; "Kincaid"; "Larry" ]
    (students |> List.findIndex ((=) student)) * 2

let getPlant =
    function
    | 'C' -> Clover
    | 'G' -> Grass
    | 'R' -> Radishes
    | 'V' -> Violets

let plants (diagram: string) student =
    diagram.Split '\n'
    |> Seq.collect (Seq.skip (getOffset student) >> Seq.take 2)
    |> Seq.map getPlant
    |> List.ofSeq
