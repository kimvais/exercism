module DifferenceOfSquares

let pow2 x =
    pown x 2

let squareOfSum (number: int): int =
    [ 1 .. number ] |> Seq.sum |> pow2

let sumOfSquares (number: int): int =
    [ 1 .. number ] |> Seq.sumBy pow2

let fork f g h a = h (f a) (g a)

let differenceOfSquares = fork squareOfSum sumOfSquares (-)
