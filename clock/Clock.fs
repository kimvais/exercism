module Clock

let div x y =
    y / float x

let normalize clock =
    let ret = clock % (24 * 60)
    if ret < 0 then 24 * 60 + ret
    else ret

let div60 = div 60.0

let create hours minutes =
    (hours * 60 + minutes) |> normalize

let add minutes clock =
    clock + minutes |> normalize


let subtract minutes clock =
    clock - minutes |> normalize

let display (clock: int) =
    let hours =
        clock
        |> (float
            >> div60
            >> floor
            >> int)
        |> (fun x -> x % 24)

    let minutes = clock % 60
    sprintf "%02d:%02d" hours minutes
