module DiffieHellman

open System.Numerics
open System.Security.Cryptography

let rng = RNGCryptoServiceProvider()

// Modular exponentiation taken from https://rosettacode.org/wiki/Modular_exponentiation#F.23
let expMod a b n =
    let rec loop a b c =
        if b = 0I then
            c
        else
            loop (a * a % n) (b >>> 1)
                (if b &&& 1I = 0I then c
                 else c * a % n)
    loop a b 1I

let getRandBytes n =
    let byteArray = Array.init (n) (fun _ -> (byte) 0)
    rng.GetBytes byteArray
    byteArray

let getRandBigInt (max: bigint) =
    let size = max.GetByteCount()
    let bytes = getRandBytes size
    bigint bytes
    |> abs
    |> (fun x -> x % max)

let privateKey (primeP: BigInteger) =
    getRandBigInt primeP

let publicKey primeP primeG privateKey =
    expMod primeG privateKey primeP

let secret primeP publicKey privateKey =
    expMod publicKey privateKey primeP
