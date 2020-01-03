module Acronym

open System

let isDelim = function
    | ' ' | '_' | '-' -> true
    | _ -> false

let isAcronymChar = function
    | c when c >= 'A' && c <= 'Z' -> true
    | _ -> false

let matchTla t =
    let (d, c) = t
    (isDelim d) && (isAcronymChar c)

let abbreviate (s:string) =
    sprintf " %s" (s.ToUpper())
    |> Seq.pairwise
    |> Seq.filter matchTla
    |> Seq.map (fun (_, c) -> c)
    |> String.Concat
