module AtbashCipher

let Cipher =
    [ int 'z' .. -1 .. int 'a' ]
    |> List.map char
    |> Array.ofList

let transmuteChar (c: char) =
    let i = int (System.Char.ToLower(c)) - int 'a' 
    match c with
    | c when System.Char.IsLetter c -> Cipher.[i]
    | c when System.Char.IsDigit c -> c

// Since (en|de)coding is symmetric with extra steps in encoding, let's define decode first and re-use that in encode
let decode =
    String.filter (System.Char.IsLetterOrDigit) >> String.map transmuteChar

let splitToGroupsOfFive: string -> string =
    Seq.chunkBySize 5
    >> Seq.map System.String.Concat
    >> String.concat " "

let encode =
    decode >> splitToGroupsOfFive
