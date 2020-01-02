module ArmstrongNumbers

let isArmstrongNumber (number: int): bool =
    let ns = number.ToString()
    let toPow = ns.Length
    let getDigit = string >> int
    ns
    |> Seq.map getDigit
    |> Seq.fold (fun x y -> x + pown y toPow) 0 = number
