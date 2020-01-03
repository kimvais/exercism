﻿module VariableLengthQuantity

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

// Taken from https://stackoverflow.com/a/6737659/180174
let splitBy f input =
    let i = ref 0
    input
    |> Seq.groupBy (fun x ->
        if f x then incr i
        !i)
    |> Seq.map snd

let decode (s: seq<byte>) =
    let numbers = splitBy (fun n -> n &&& 0x80uy = 1uy) s
    seq {
        for n in numbers do
            match n |> Seq.tryFind (lastByte) with
            | None -> failwith "Non-terminating sequence"
            | Some _ -> 
                let values = Seq.rev n |> Seq.mapi calcByteVal
                values |> Seq.sum
    }
    |> Seq.toList
    |> Some
// s |> Seq.takeWhile moreBytesFollow
