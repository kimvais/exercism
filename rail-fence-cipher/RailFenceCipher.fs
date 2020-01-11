module RailFenceCipher

let getRailNumber numRails =
    seq {
        while true do
            yield! [ 0 .. numRails - 1 ]
            yield! [ numRails - 2 .. -1 .. 1 ]
    }

let charIndexes rails length =
    Seq.mapi (fun i r -> (r, i)) (getRailNumber rails)
    |> Seq.take length
    |> Seq.sortBy fst

let encode rails (message: string) =
    Seq.zip message (getRailNumber rails)
    |> Seq.sortBy snd // Seq.sortBy is stable.
    |> Seq.map fst
    |> System.String.Concat

let decode rails message =
    let indices = charIndexes rails (String.length message)
    Seq.zip message indices
    |> Seq.sortBy (snd >> snd)
    |> Seq.map fst
    |> System.String.Concat
