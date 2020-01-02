module ProteinTranslation

type Protein = | Cysteine | Leucine | Methionine | Phenylalanine | Serine | Tryptophan | Tyrosine | STOP

let getProtein codon =
    match codon with
    | "AUG" -> Methionine
    | "UUU" | "UUC" -> Phenylalanine
    | "UUA" | "UUG" -> Leucine
    | "UCU" | "UCC" | "UCA" | "UCG" -> Serine
    | "UAU" | "UAC" -> Tyrosine
    | "UGU" | "UGC" -> Cysteine
    | "UGG" -> Tryptophan
    | "UAA" | "UAG" | "UGA" -> STOP
    | _ -> failwith (sprintf "Unknown codon %A" codon)

let proteins (rna: string) =
    rna
    |> Seq.chunkBySize 3
    |> Seq.map (System.String >> getProtein)
    |> Seq.takeWhile ((<>) STOP)
    |> Seq.map (fun c -> c.ToString())
    |> Seq.toList
