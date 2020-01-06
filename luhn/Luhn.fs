module Luhn

let getDigitValue i c =
    let n = c |> string |> int
    match i % 2 with
    | 1 ->
        match n * 2 with
        | x when x > 9 -> x - 9
        | x -> x
    | _ -> n

let valid number =
    let input = String.filter (fun c -> c <> ' ') number
    match (String.forall System.Char.IsDigit input) && String.length input > 1 with
    | false -> false
    | true ->
        (input
         |> Seq.rev
         |> Seq.mapi getDigitValue
         |> Seq.sum) % 10 = 0
