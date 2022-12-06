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
      "the house"
      "Jack built."
      ] |> List.chunkBySize 2 |> List.rev


let preample = sprintf "This is %s"


let verse (a, b) = sprintf "%s that %s" a b

let rec reciteVerse acc verses =
    match verses with
    | [] -> $"This is%s{acc}"
    | [[n;v]] -> reciteVerse $" %s{n} that %s{v}%s{acc}" []
    | l ->
        let [n;v] = List.head l
        reciteVerse $" %s{n} that %s{v}%s{acc}" (List.tail l)
    

    
let recite startVerse endVerse =
    [startVerse..endVerse] |> List.map (fun n -> reciteVerse "" (input |> List.take n ))