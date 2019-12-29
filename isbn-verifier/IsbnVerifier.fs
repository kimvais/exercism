module IsbnVerifier

open System.Text.RegularExpressions

let isValid (isbn: string): bool =
    let cleanedUpIsbn = isbn |> Seq.filter (fun c -> c <> '-')
    match Regex.IsMatch(cleanedUpIsbn |> System.String.Concat, "^[0-9]{9}[0-9|X]$") with
    | true ->
        let calcDigit (acc: int, pos: int) (n: char) =
            let digit =
                match n with
                | 'X' -> 10
                | n -> n |> (string >> int)
            (acc + pos * digit, pos + 1)

        let checksum, _ =
            cleanedUpIsbn
            |> Seq.rev
            |> Seq.fold calcDigit (0, 1)

        checksum % 11 = 0
    | false -> false
