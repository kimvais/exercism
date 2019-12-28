module RnaTranscription

let toRna (dna: string): string =
    let rna c =
        match c with
        | 'G' -> "C"
        | 'C' -> "G"
        | 'T' -> "A"
        | 'A' -> "U"
    dna |> Seq.map rna |> String.concat ""
