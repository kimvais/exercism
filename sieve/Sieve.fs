module Sieve

let primes limit =
    let inner lim =
        seq {

            let mutable eliminated = Set.empty<int>


            let eliminate number =
                for multiple in [ 2 * number .. number .. lim ] do
                    eliminated <- eliminated.Add multiple
                eliminated |> Set.filter (fun n -> n > number)

            for candidate in [ 2 .. lim ] do
                if not (eliminated.Contains candidate) then
                    eliminated <- eliminate candidate
                    yield candidate
        }
    inner limit |> Seq.toList
