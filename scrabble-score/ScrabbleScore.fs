module ScrabbleScore

open System

let scoreLetter = function
    | 'a' | 'e' | 'i' | 'o' | 'u'| 'l' | 'n' | 'r' | 's' | 't' -> 1
    | 'd' | 'g'                                                -> 2
    | 'b' | 'c' | 'm' | 'p'                                    -> 3
    | 'f' | 'h' | 'v' | 'w' | 'y'                              -> 4
    | 'k'                                                      -> 5
    | 'j' | 'x'                                                -> 8
    | 'q' | 'z'                                                -> 10
    | _                                                        -> 0

let score: seq<char> -> int =
    Seq.sumBy (Char.ToLower >> scoreLetter) 