module Poker


type Suite =
    | Hearts
    | Spades
    | Diamonds
    | Clubs

type Value =
    | Ace = 14
    | King = 13
    | Queen = 12
    | Jack = 11
    | Ten = 10
    | Nine = 9
    | Eight = 8
    | Seven = 7
    | Six = 6
    | Five = 5
    | Four = 4
    | Three = 3
    | Two = 2


type Card = { Value: Value; Suite: Suite }

type Hand = Hand of Card array

let parseCard (s: string) =
    let (value: Value) =
        match s.[0] with
        | 'A' -> Value.Ace
        | 'K' -> Value.King
        | 'Q' -> Value.Queen
        | 'J' -> Value.Jack
        | 'T' -> enum<Value> 10
        | n -> n |> string |> int |> enum<Value>

    let suite =
        match s.[1] with
        | 'H' -> Hearts
        | 'S' -> Spades
        | 'D' -> Diamonds
        | 'C' -> Clubs
        | _ -> failwith "Unrecognized suite"

    { Value = value; Suite = suite }

let formatCard c = $"<%A{c.Value} of %A{c.Suite}>"

let parseHand (h: string) =
    let hand =
        h.Split ' ' |> Seq.map parseCard |> Seq.sortBy (fun c -> c.Value) |> Seq.rev

    hand |> Seq.iter (formatCard >> printfn "%s")
