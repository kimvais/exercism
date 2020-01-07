module Etl

// Not very idiomatic functional code, but it works and is readable...

let transform (input: Map<int, char list>): Map<char, int> =
    let helper score letters =
        seq {
            for letter in letters do
                yield (letter |> System.Char.ToLower, score)
        }
    seq {
        for (s, ls) in input |> Map.toSeq do
            yield! helper s ls
    }
    |> Map.ofSeq
