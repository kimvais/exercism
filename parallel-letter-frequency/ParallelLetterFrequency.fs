module ParallelLetterFrequency

open System

let countFrequency text =
    async {
        return text
               |> Seq.filter (fun (c: char) -> Char.IsLetter c)
               |> Seq.map Char.ToLower
               |> Seq.countBy id
    }

let frequency texts =
    texts
    |> List.map countFrequency
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Seq.concat
    |> Seq.groupBy fst
    |> Seq.map (fun (letter, counts) -> letter, (counts |> Seq.sumBy snd))
    |> Map.ofSeq
