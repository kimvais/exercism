module Anagram

let sort (s: string) =
    Seq.sort s |> System.String.Concat

let compareWords (a: string) (b: string) =
    let a', b' = a.ToLower(), b.ToLower()
    a' <> b' && sort a' = sort b'

let findAnagrams sources target =
    List.filter (compareWords target) sources
