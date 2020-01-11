module RotationalCipher

open System


let rotChar (key:int) (c:char) =
    let rotWithOffset offset =
        let mod26 x = x % 26
        // Same as char ((int (c) - offset + key + 26) % 26 + offset), but now with less parentheses!
        int >> (+) -offset >> (+) key >> (+) 26 >> mod26 >> (+) offset >> char
    match c with
    | x when Char.IsLower(x) -> rotWithOffset (int 'a') c
    | x when Char.IsUpper(x) -> rotWithOffset (int 'A') c
    | _ -> c
    
let rotate shiftKey text =
    String.map (rotChar shiftKey) text