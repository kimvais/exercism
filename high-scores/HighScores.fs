module HighScores

let scores = id

let latest = List.last

let personalBest = List.max

let personalTopThree: int list -> int list =
    List.sortDescending >> List.truncate 3
