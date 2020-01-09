module SimpleCipher

open System.Security.Cryptography


let rng min max = RandomNumberGenerator.GetInt32(min, max)
let keyLen = 100

let makeRandomKey =
    [ 1 .. keyLen ]
    |> Seq.map (fun _ -> char (rng (int 'a') (int 'z') + 1))
    |> System.String.Concat

let offset = int 'a'

let transcodeChar f (k: char) (c: char) =
    let c' = f (int c - offset) (int k - offset)
    // Need to handle this because modulo of negative number gives incorrectly a negative number
    match c' with
    | x when x < 0 -> char (offset + ((x + 26) % 26))
    | x -> char (offset + (x % 26))

let getKeyChar (k: string) i =
    k.[i % String.length k]

let transcode f k t = 
    let getKey = getKeyChar k
    t |> String.mapi (fun i c -> f (getKey i) c)
    
let encode = transcode (transcodeChar (+))

let decode = transcode (transcodeChar (-))

type SimpleCipher(key: string) =

    member val Key = key

    member __.Encode(plaintext) = encode key plaintext

    member __.Decode(ciphertext) = decode key ciphertext

    new() = SimpleCipher(makeRandomKey)
