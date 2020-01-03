module VariableLengthQuantity

let intToBinary (i: uint32) =
    match i with
    | 0u -> None
    | _ -> Some(byte (i % 2u), (i / 2u))

let bitValue i n =
    n <<< i

let bitSeqToInt (s: seq<byte>) =
    s
    |> Seq.mapi bitValue
    |> Seq.sum

let intToSeqOf7BitInts (s: uint32) =
    Seq.unfold intToBinary s
    |> Seq.chunkBySize 7
    |> Seq.rev
    |> Seq.map bitSeqToInt

let encode (numbers: uint32 list) =
    seq {
        for n in numbers do
            let s =
                intToSeqOf7BitInts n
                |> Seq.rev
                |> Seq.toList
            match s with
            | last :: rest ->
                yield (rest
                       |> List.rev
                       |> List.map (fun i -> i ||| 0x80uy))
                      @ [ last ]
            | [] -> yield [ 0uy ]
    }
    |> Seq.concat
    |> Seq.toList

let moreBytesFollow =
    (&&&) 0x80uy >> (<>) 0uy

let calcByteVal i b =
    (<<<) ((uint32 b) &&& 0x7fu) (i * 7)

let lastByte = moreBytesFollow >> not

let rec split f (bytes : seq<byte>) =
    let input = List.ofSeq bytes
    let heads = input |> List.takeWhile f
    match input |> List.skip (heads |> List.length) with
    | last::rest -> [heads @ [last]] @ split f rest
    | _ -> []

let splitAfterLastByte = split moreBytesFollow 

let decode (s: seq<byte>) =
    match s |> Seq.tryFind (lastByte) with
        | None -> None
        | Some _ -> splitAfterLastByte s |> Seq.map (Seq.rev >> Seq.mapi calcByteVal >> Seq.sum) |> Seq.toList |> Some
