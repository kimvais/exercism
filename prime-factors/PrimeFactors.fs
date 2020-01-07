module PrimeFactors

let factors number =
    let divisor = ref 2
    seq {
        let mutable remainder = number
        while remainder > 1L do
            if remainder % int64 !divisor = 0L then
                remainder <- remainder / int64 !divisor
                yield !divisor
            else incr divisor
    } |> Seq.toList
