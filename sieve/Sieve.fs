module Sieve

type State =
    | Prime
    | Composite

let eliminate (state: State array) candidate limit =
    [ 2 * candidate .. candidate .. limit ] |> Seq.iter (fun i -> state.[i] <- Composite)

let findNextCandidate (state: State array) candidate limit =
    [ candidate + 1 .. limit ]
    |> Seq.skipWhile (fun idx -> state.[idx] = Composite)
    |> Seq.tryHead

let doSieve limit =
    seq {
        let candidates = Array.create (limit + 1) Prime
        let mutable candidate = 2
        while candidate <= limit && candidate > 0 do
            yield candidate
            eliminate candidates candidate limit
            match findNextCandidate candidates candidate limit with
            | None -> candidate <- -1
            | Some n -> candidate <- n
    }

let primes limit = doSieve limit |> Seq.toList
