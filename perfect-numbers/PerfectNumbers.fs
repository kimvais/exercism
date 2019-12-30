module PerfectNumbers

type Classification =
    | Perfect
    | Abundant
    | Deficient

let factors n =
    [ 1 .. n / 2 ] |> Seq.filter (fun i -> n % i = 0)

let alinquot n = factors n |> Seq.sum

let classify n: Classification option =
    match n with
    | n when n < 1 -> None
    | _ ->
        match alinquot n with
        | a when a = n -> Some Perfect
        | a when a > n -> Some Abundant
        | _ -> Some Deficient
