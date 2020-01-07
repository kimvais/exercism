module AllYourBase

let rebase digits inputBase outputBase =
    let unfolder baseN n =
        match n with
        | x when x <= 0 -> None
        | _ -> Some(n % baseN, n / baseN)
    match Seq.forall (fun n -> n >= 0 && n < inputBase) digits && inputBase > 1 && outputBase > 1 with
    | false -> None
    | true ->
        Seq.rev digits
        |> Seq.mapi (fun i d -> d * pown inputBase i)
        |> Seq.sum
        |> Seq.unfold (unfolder outputBase)
        |> Seq.rev
        |> Seq.toList
        |> function
        | [] -> Some [ 0 ]
        | x -> Some x
