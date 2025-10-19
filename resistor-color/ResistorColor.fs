module ResistorColor

let colors: string list = ["black"; "brown"; "red"; "orange"; "yellow"; "green"; "blue"; "violet"; "grey"; "white"]

let colorCode (color: string): int =
    match color with
    | "black" -> 0
    | "brown" -> 1
    | "red" -> 2
    | "orange" -> 3
    | "yellow" -> 4
    | "green" -> 5
    | "blue" -> 6
    | "violet" -> 7
    | "grey" -> 8
    | "white" -> 9
    | _ -> failwith "Not a valid color."
