module PigLatin

open System.Text.RegularExpressions

let (|RegexMatchGroups|_|) pattern input =
    let m = Regex.Match(input, pattern)

    if m.Success then
        Some m.Groups
    else
        None

let translateWord input =
    match input with
    | RegexMatchGroups "^([aeiou]|yt|xr)(.+)$" _ -> sprintf "%say" input
    | RegexMatchGroups "^([bcdfghjklmnprstvxz]?qu)(.+)" g -> sprintf "%s%say" g.[2].Value g.[1].Value
    | RegexMatchGroups "^(?<prefix>[bcdfghjklmnpqrstvxyz]+)(?<suffix>[aeiouy].+)" g ->
        sprintf "%s%say" g.["suffix"].Value g.["prefix"].Value
    | RegexMatchGroups "(.)y" g -> sprintf "y%say" g.[1].Value

let translate (input: string) =
    input.Split " "
    |> Array.map translateWord
    |> String.concat " "
