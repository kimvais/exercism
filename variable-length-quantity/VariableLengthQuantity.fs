module VariableLengthQuantity
open System

let intToBinary (i:uint32) = 
    match i with
    | 0u -> None
    | _ -> Some (byte (i % 2u), (i / 2u))

let bitValue i n =
    n <<< i

let bitSeqToInt (s:seq<byte>) =
    s|> Seq.mapi bitValue |> Seq.sum
    
let intToSeqOf7BitInts (s:uint32) =
   Seq.unfold intToBinary s |> Seq.chunkBySize 7 |> Seq.rev |> Seq.map bitSeqToInt 
    
let encode (numbers: uint32 list) =
    let s = numbers |> List.map (fun n -> intToSeqOf7BitInts n) |> Seq.concat |> Seq.rev |> Seq.toList
    let last = s.[0]
    let rest = s.[1..] |> List.rev
    (rest |> List.map (fun i -> i ||| 0x80uy)) @ [last]
    
let decode s = None