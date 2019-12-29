module AffineCipher

let tryFindMMI m a =
    seq {
        for i in [ 0 .. m ] do
            if ((a % m) * i) % m = 1 then yield i
    }
    |> Seq.tryHead

let createCodec f m a =
    match tryFindMMI m a with
    | Some _ -> f
    | None -> raise (System.ArgumentException(sprintf "%d and %d are not co-primes" a m))

let E m a b x =
    let f = (a * x + b) % m
    createCodec f m a

let D m a b y =
    let f a = a * (y - b) % m
    createCodec f m a

let A = int 'a'
let charToInt (c: char) =
    int c - A
let intToChar c =
    char (c + A)

let toLower (c: string) =
    c.ToLower()

let cleanUp (text: string) =
    text
    |> toLower
    |> String.filter (fun c -> System.Char.IsLower(c) || System.Char.IsDigit(c))



let codeText codec text =
    text
    |> cleanUp
    |> Seq.map
        (charToInt
         >> codec
         >> intToChar)
    |> System.String.Concat

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
    cipheredText
    |> cleanUp
    |> Seq.map
        (charToInt
         >> decoder
         >> intToChar)
    |> System.String.Concat

let encode a b plainText =
    let encoder = E 26 a b
    plainText
    |> cleanUp
    |> Seq.map
        (charToInt
         >> encoder
         >> intToChar)
    |> postProcess
