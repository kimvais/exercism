module SquareRoot

let squareRoot n =
    Seq.initInfinite id |> Seq.skipWhile (fun i -> pown i 2 < n) |> Seq.head
