module SqueakyClean

open System

let transform (c: char) : string =
    match c with
    | '-' -> "_"
    | ' ' -> ""
    | s when 'α' <= s && s <= 'ω' -> "?"
    | s when (Char.IsDigit s) -> ""
    | s when (Char.IsUpper s) -> $"-{Char.ToLower s}"
    | s -> string s
    
let clean (identifier: string): string =
    identifier |> String.collect transform
    