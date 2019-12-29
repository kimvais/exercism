module Darts

let score (x: double) (y: double): int =
    match sqrt (x ** 2.0 + y ** 2.0) with
    | d when d <= 1.0 -> 10
    | d when d <= 5.0 -> 5
    | d when d <= 10.0 -> 1
    | _ -> 0
