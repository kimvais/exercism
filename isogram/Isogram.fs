module Isogram

open System

let isIsogram (str: string) =
    let word = str.ToLower() |> String.filter Char.IsLower
    (word |> Seq.distinct |> Seq.length) = (word |> String.length)
