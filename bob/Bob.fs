module Bob

open System.Text.RegularExpressions

let stripChars text (chars: string) =
    Array.fold (fun (s: string) c -> s.Replace(c.ToString(), "")) text (chars.ToCharArray())

let Silence = Regex "^\w*$"
let Yelling = Regex "\w*[^A-Z]+\w*$"
let Question = Regex ".*\?\w*$"
let YelledQuestion = Regex "\w*[A-Z]+\w*\?$"

let response (input: string): string =
    let text = stripChars input "\n"
    match text with
    | x when YelledQuestion.IsMatch(x) -> "Calm down, I know what I'm doing!"
    | x when Question.IsMatch(x) -> "Sure."
    | x when Yelling.IsMatch(x) -> "Whoa, chill out!"
    | x when Silence.IsMatch(x) -> "Fine. Be that way!"
    | _ -> "Whatever."
