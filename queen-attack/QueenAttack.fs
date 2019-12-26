module QueenAttack

open System.Buffers

let create (position: int * int) =
    let invalid z =
        z < 0 || z > 7
    match position with
    | (x, y) when invalid x || invalid y -> false
    | _ -> true

let canAttack (queen1: int * int) (queen2: int * int) =
    let (x1, y1) = queen1
    let (x2, y2) = queen2
    let xdist = abs (x1 - x2)
    let ydist = abs (y1 - y2)
    if x1 = x2 then true
    elif y1 = y2 then true
    elif xdist = ydist then true
    else false
