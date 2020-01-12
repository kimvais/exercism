module Change

let findFewestCoins coins target =
    let getChange =
        seq {
            let mutable toGo = target
            while toGo > 0 do
                let coin =
                    coins
                    |> Seq.filter (fun x -> x <= toGo)
                    |> Seq.max
                toGo <- toGo - coin
                yield coin
        }
    match target with
    | x when x < 0 -> None
    | x when x < (coins |> Seq.min) -> None
    | _ ->
        getChange
        |> Seq.sort
        |> Seq.toList
        |> Some
