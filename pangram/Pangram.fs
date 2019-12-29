module Pangram

let alphabet = set [ 'a' .. 'z' ]

let isPangram (input: string): bool =
    alphabet.IsSubsetOf
        (input.ToLower()
         |> set)
