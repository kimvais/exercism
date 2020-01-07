module PythagoreanTriplet

let findTriplet sum =
    seq {
        for b in [ 1 .. sum / 2 ] do
            for a in [ 1 .. b ] do
                let c = sum - (b + a)
                if pown a 2 + pown b 2 = pown c 2 then yield (a, b, c)
    }

let tripletsWithSum n = findTriplet n |> List.ofSeq |> List.sort
