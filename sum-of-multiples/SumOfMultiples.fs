module SumOfMultiples

let getMultiples limit n =
    match n with
    | 0 -> Seq.empty
    | _ ->
        seq {
            let mulN x = n * !x
            let m = ref 0
            while mulN m < limit do
                yield mulN m
                incr m
        }

let sum (numbers: int list) (upperBound: int): int =
    numbers
    |> Seq.map (getMultiples upperBound)
    |> (Seq.concat >> Seq.distinct >> Seq.sum)
