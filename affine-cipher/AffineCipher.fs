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

let E m a b x =
    let f _ = (a * x + b) % m
    createCodec f m a

let D m a b x =
    let f a' =
        let ret = (a' * (x - b)) % m
        match ret with
        | n when n < 0 -> n + m
        | _ -> ret
    createCodec f m a

let toLower (c: string) =
    c.ToLower()

let cleanUp (text: string) =
    text
    |> toLower
    |> String.filter (fun c -> System.Char.IsLower(c) || System.Char.IsDigit(c))

let handleChar codec c =
    charToInt
    >> codec
    >> intToChar

let postProcess (chars: seq<char>) =
    chars
    |> Seq.chunkBySize 5
    |> Seq.map (fun f -> f |> System.String.Concat)
    |> String.concat " "

let decode a b cipheredText =
    let decoder = D 26 a b
    let decode' =
        (charToInt
         >> decoder
         >> intToChar)
    cipheredText
    |> cleanUp
    |> Seq.map decode'
    |> System.String.Concat

let encode a b plainText =
    let encoder = E 26 a b

    let encode' c =
        match c with
        | t when t >= '0' && '9' >= t -> t
        | t ->
            t
            |> (charToInt
                >> encoder
                >> intToChar)
    plainText
    |> cleanUp
    |> Seq.map encode'
    |> postProcess
