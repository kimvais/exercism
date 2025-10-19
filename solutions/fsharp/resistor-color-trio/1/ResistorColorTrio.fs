module ResistorColorTrio

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

let label colors =
    let [a; b; c] = colors |> List.take 3 |> List.map colorCode
    let n = a * 10 + b
    let p = pown c 10
    match p with
    | 0 -> $"{n} ohms"
    | 1 -> $"{n}0 ohms"
    | 2 -> $"{n}00 ohms"
    | 3 -> $"{n} kiloohms"
    | 4 -> $"{n}0 kiloohms"
    | 5 -> $"{n}00 kiloohms"
    | 6 -> $"{n} megaohms"
    | 7 -> $"{n}0 megaohms"
    | 8 -> $"{n}00 megaohms"
    | 9 -> $"{n} gigaohms"
    | _ -> failwith "Invalid value"

