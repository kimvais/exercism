module Bob

open System

let stripChars text (chars: string) =
    Array.fold (fun (s: string) c -> s.Replace(c.ToString(), "")) text (chars.ToCharArray())

let (|YelledQuestion|Question|Yelling|Statement|Silence|) (phrase: string) =
    match phrase with
    | x when x |> Seq.isEmpty -> Silence
    | x when x.ToUpper() = x && (x |> Seq.exists Char.IsLetter) ->
        match x with
        | y when Seq.last y = '?' -> YelledQuestion
        | _ -> Yelling
    | x when Seq.last x = '?' -> Question
    | _ -> Statement

let response (input: string): string =
    match (input |> String.filter (Char.IsWhiteSpace >> not)) with
    | YelledQuestion -> "Calm down, I know what I'm doing!"
    | Yelling -> "Whoa, chill out!"
    | Question -> "Sure."
    | Statement -> "Whatever."
    | Silence -> "Fine. Be that way!"
