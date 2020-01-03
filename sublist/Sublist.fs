module Sublist

type SublistType = Equal | Sublist | Superlist | Unequal

let l = List.length 
let checkForSublist (super: 'a list)  (sub: 'a list) retValue =
    match l sub with 
    | 0 -> retValue
    | _ -> 
        super |> Seq.windowed (l sub) |> Seq.tryFind ((=) (sub |> Array.ofList)) |> function
        | Some _ -> retValue
        | None -> Unequal
   
let sublist xs ys =
    match (xs, ys) with
    | (x, y) when l x > l y -> checkForSublist x y Superlist
    | (x, y) when l x < l y -> checkForSublist y x Sublist
    | (x, y) when l x = l y -> x = y |> function
        | true -> Equal
        | false -> Unequal
    | _ -> Unequal
    