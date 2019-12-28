module CollatzConjecture

let calcNext num =
    match num with
    | 1 -> None
    | n when n % 2 = 0 -> Some(num, n / 2)
    | n -> Some(num, 3 * n + 1)

let steps (number: int): int option =
    match number with
    | n when n > 0 -> Some (Seq.unfold calcNext number |> Seq.length)
    | _ -> None
