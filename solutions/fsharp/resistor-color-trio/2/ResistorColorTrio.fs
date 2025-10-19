module ResistorColorTrio

let colorCode (color: string) : int =
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
    let [ a; b; c ] = colors |> List.take 3 |> List.map colorCode

    let ohms, zeroes =
        match a, b with
        | 0, 0 -> 0, 0
        | a', 0 -> a', c + 1
        | a', b' -> a' * 10 + b', c

    match zeroes with
    | 0 -> $"{ohms} ohms"
    | 1 -> $"{ohms}0 ohms"
    | 2 -> $"{ohms}00 ohms"
    | 3 -> $"{ohms} kiloohms"
    | 4 -> $"{ohms}0 kiloohms"
    | 5 -> $"{ohms}00 kiloohms"
    | 6 -> $"{ohms} megaohms"
    | 7 -> $"{ohms}0 megaohms"
    | 8 -> $"{ohms}00 megaohms"
    | 9 -> $"{ohms} gigaohms"
    | _ -> failwith "Invalid value"
