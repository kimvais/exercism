module RunLengthEncoding

open System.Text.RegularExpressions

let encodeOne (m: Match) =
    let len = String.length m.Value
    let c = m.Value.[0]
    match len with
    | 1 -> string c
    | _ -> sprintf "%d%c" len c

let decodeOne (m: Match) =
    let len = m.Groups.[1].Value
    let c = m.Groups.[2].Value
    match len with
    | "" -> string c
    | _ -> String.init (int len) (fun _ -> c)

let transform regex mapfun input =
    Regex(regex).Matches(input)
    |> Seq.map mapfun
    |> System.String.Concat

let encode = transform "((.)\2*)" encodeOne
let decode = transform "(\d*)(\D)" decodeOne
