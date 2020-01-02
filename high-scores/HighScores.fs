module HighScores

let scores (values: int list): int list = values

let latest (values: int list): int =
    values |> List.last

let personalBest (values: int list): int =
    values |> List.max

let personalTopThree (values: int list): int list =
    values |> List.sort |> List.rev |> function
        | n when n |> List.length > 3 -> n |> List.take 3
        | n -> n
