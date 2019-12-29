module AffineCipher

let A = int 'a'
let charToInt (c: char) =
    int c - A
let intToChar c =
    char (c + A)

let tryFindMMI m a =
    seq {
        for i in [ 0 .. m ] do
            if ((a % m) * i) % m = 1 then yield i
    }
    |> Seq.tryHead

let createCodec f m a =
    match tryFindMMI m a with
    | Some a' -> f a'
    | None -> raise (System.ArgumentException(sprintf "%d and %d are not co-primes" a m))

let encoder m a b x =
    let f _ = (a * x + b) % m
    createCodec f m a

let decoder m a b x =
    let f a' =
        let ret = (a' * (x - b)) % m
        // F# chose to keep the bug in .NET that modulo of a negative number is negative, need to work around that...
        match ret with
        | n when n < 0 -> n + m
        | _ -> ret
    createCodec f m a

let cleanUp (text: string) =
    let toLower (c: string) =
        c.ToLower()
    text
    |> toLower
    |> String.filter (fun c -> System.Char.IsLower(c) || System.Char.IsDigit(c))

let postProcess (chars: seq<char>) =
    chars
    |> Seq.chunkBySize 5
    |> Seq.map (fun f -> f |> System.String.Concat)
    |> String.concat " "

let handleChar codec c =
    match c with
    | t when t >= '0' && '9' >= t -> t
    | t ->
        t
        |> (charToInt
            >> codec
            >> intToChar)

let decode a b cipheredText =
    cipheredText
    |> cleanUp
    |> Seq.map (handleChar (decoder 26 a b))
    |> System.String.Concat

let encode a b plainText =
    plainText
    |> cleanUp
    |> Seq.map (handleChar (encoder 26 a b))
    |> postProcess
