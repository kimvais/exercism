module TwoFer

let twoFer (input: string option): string = 
    match input with
    | Some input -> input 
    | None -> "you"
    |> sprintf "One for %s, one for me."
