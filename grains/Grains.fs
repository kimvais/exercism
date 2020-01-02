module Grains

open System.Numerics

let squareGrains n =
    BigInteger.Pow(2I, (n - 1)) 
    
let square (n: int): Result<uint64,string> =
    match n with
    | n when n > 0 && n < 65 -> Ok (squareGrains n |> uint64)
    | _ ->  Error "square must be between 1 and 64"
    
let total: Result<uint64,string> =
    Ok ((squareGrains 65) - 1I |> uint64)
