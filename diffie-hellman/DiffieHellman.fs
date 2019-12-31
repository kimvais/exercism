module DiffieHellman

open System.Numerics
open System.Security.Cryptography

let rng = RNGCryptoServiceProvider()

let expMod a b n =
    BigInteger.ModPow(a, b, n)

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
