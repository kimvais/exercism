module KindergartenGarden

type Plant = | Clover | Grass | Radishes | Violets

let students =
    [ "Alice"; "Bob"; "Charlie"; "David"; "Eve"; "Fred"; "Ginny"; "Harriet"; "Ileana"; "Joseph"; "Kincaid"; "Larry" ]

let getPlant = function
    | 'C' -> Clover
    | 'G' -> Grass
    | 'R' -> Radishes
    | 'V' -> Violets

let plants (diagram: string) student =
    let rows = diagram.Split '\n'
    let i = (students |> List.findIndex ((=) student)) * 2
    // If it's stupid but it works...
    [ rows.[0].[i]
      rows.[0].[i + 1]
      rows.[1].[i]
      rows.[1].[i + 1] ]
    |> List.map getPlant
