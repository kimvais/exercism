module WordCount

open System.Text.RegularExpressions

let word = Regex("[a-z]+'[a-z]+|[a-z0-9]+")

let countWords (phrase: string) =
    word.Matches(phrase.ToLower())
    |> Seq.countBy (fun m -> m.Value)
    |> Map.ofSeq
