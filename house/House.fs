module House

let input =
    [ "the horse and the hound and the horn"
      "belonged to"
      "the farmer sowing his corn"
      "kept"
      "the rooster that crowed in the morn"
      "woke"
      "the priest all shaven and shorn"
      "married"
      "the man all tattered and torn"
      "kissed"
      "the maiden all forlorn"
      "milked"
      "the cow with the crumpled horn"
      "tossed"
      "the dog"
      "worried"
      "the cat"
      "killed"
      "the rat"
      "ate"
      "the malt"
      "lay in"
      ] 


let preample = sprintf "This is %s"


let verse (a, b) = sprintf "%s that %s" a b

let rec reciteVerse v x =
    match x with
    | 0 -> preample v
    | n ->
        let noun = input.[n]
        let verb = input.[n - 1]
        reciteVerse $"%s{verse (noun, verb)} %s{v}" (n - 1)
    
let recite startVerse endVerse =
    [startVerse .. endVerse] |> List.map (fun v -> reciteVerse "the house that Jack built." (v- 1))
