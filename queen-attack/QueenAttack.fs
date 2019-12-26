module QueenAttack

open System.Buffers

let create (position: int * int) = 
    let (x, y) = position
    if x < 0 || y < 0 || x > 7 || y > 7 then false
    else true
    
let canAttack (queen1: int * int) (queen2: int * int) =
    let (x1, y1) = queen1
    let (x2, y2) = queen2
    let xdist = abs(x1 - x2)
    let ydist = abs(y1 - y2)
    if x1 = x2 then true
    elif y1 = y2 then true
    elif xdist = ydist then true
    else false