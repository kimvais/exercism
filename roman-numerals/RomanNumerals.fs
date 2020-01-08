module RomanNumerals

let matchDigit =
    function
    // These could be generated programmatically, but that's probably more code than defining them like this.
    | 3000 -> "MMM"
    | 2000 -> "MM"
    | 1000 -> "M"
    | 900 -> "CM"
    | 800 -> "DCCC"
    | 700 -> "DC"
    | 600 -> "DC"
    | 500 -> "D"
    | 400 -> "CD"
    | 300 -> "CCC"
    | 200 -> "CC"
    | 100 -> "C"
    | 90 -> "XC"
    | 80 -> "LXXX"
    | 70 -> "LXX"
    | 60 -> "LX"
    | 50 -> "L"
    | 40 -> "XL"
    | 30 -> "XXX"
    | 20 -> "XX"
    | 10 -> "X"
    | 9 -> "IX"
    | 8 -> "VIII"
    | 7 -> "VII"
    | 6 -> "VI"
    | 5 -> "V"
    | 4 -> "IV"
    | 3 -> "III"
    | 2 -> "II"
    | 1 -> "I"
    | 0 -> ""
    | _ -> failwith "Can not convert to roman numeral."

let roman number =
    let unfolder n =
        match n with
        | x when x <= 0 -> None
        | _ -> Some(n % 10, n / 10)
    Seq.unfold unfolder number
    |> Seq.mapi (fun i n -> n * (pown 10 i))
    |> Seq.rev
    |> Seq.map matchDigit
    |> System.String.Concat
