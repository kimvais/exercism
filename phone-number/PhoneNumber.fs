module PhoneNumber

open System
open System.Text.RegularExpressions

type Code = | AREA = 0 | EXCHANGE = 3

let sanitize =
    let charsToStrip = set "+() .-"
    String.filter (fun c -> (not (charsToStrip.Contains c))) >> Ok

let validateNoLetters s =
    match String.forall (Char.IsLetter >> not) s with
    | true -> Ok s
    | false -> Error "letters not permitted"

let validateOnlyDigits s =
    match String.forall Char.IsDigit s with
    | true -> Ok s
    | false -> Error "punctuations not permitted"

let validateLength s =
    match String.length s with
    | 10 -> Ok s
    | 11 ->
        match s.[0] with
        | '1' -> Ok(s.Substring 1)
        | _ -> Error "11 digits must start with 1"
    | x when x > 11 -> Error "more than 11 digits"
    | _ -> Error "incorrect number of digits"

let validateCode (cType: Code) (s: string) =
    let numberNames = [ "zero"; "one" ]
    let number = s.[int cType] |> string |> int
    match number with
    | 0
    | 1 -> Error(sprintf "%s code cannot start with %s" ((string cType).ToLower()) numberNames.[number])
    | _ -> Ok s
    
let convertToUInt64 s =
    let validNumber = Regex("([2-9]\d{2}[2-9]\d{6})")
    match validNumber.Match(s).Groups.[1].Value with
    | "" -> Error "not a valid phone number"
    | x -> Ok (uint64 x)
    
let clean: string -> Result<uint64, string> =
    sanitize 
    >> Result.bind validateLength
    >> Result.bind validateNoLetters
    >> Result.bind validateOnlyDigits
    >> Result.bind (validateCode Code.AREA)
    >> Result.bind (validateCode Code.EXCHANGE)
    >> Result.bind convertToUInt64
