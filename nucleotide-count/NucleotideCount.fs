module NucleotideCount

let validNucleotides = seq "ACGT"
let validNucleotidesSet = set validNucleotides
let checkIfValid c =
    not (validNucleotidesSet.Contains c)

let nucleotideCounts (strand: string): Option<Map<char, int>> =
    match strand
          |> Seq.filter checkIfValid
          |> Seq.isEmpty with
    | true ->
        let counts =
            strand
            |> Seq.countBy id
            |> Map.ofSeq

        let getNumber n =
            match counts.TryFind(n) with
            | Some c -> (n, c)
            | None -> (n, 0)

        validNucleotides
        |> Seq.map getNumber
        |> Map.ofSeq
        |> Some
    | false -> None
