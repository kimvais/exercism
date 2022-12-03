module Poker


type Suite =
    | Clubs
    | Diamonds
    | Spades
    | Hearts

type Value =
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace


type Card = { Value: Value; Suite: Suite }

type Hand' = Hand' of Card array

type StraightFlush = { High: Card }
type FourOfAKind = { Value: Value; Kicker: Card }
type FullHouse = { Triplet: Value; Pair: Value }
type Flush = { High: Card; Kickers: Card array }
type Straight = { High: Value }
type ThreeOfAKind = { Triplet: Value; Kickers: Card array }
type TwoPair = { HighPair: Value; LowPair: Value; Kicker: Value }
type Pair = { Pair: Value; Kickers: Card array }
type HighCard = { High: Value; Kickers: Card array }




type Hand =
    | StraightFlush of StraightFlush
    | FourOfAKind of FourOfAKind
    | FullHouse of FullHouse
    | Flush of Flush
    | Straight of Straight
    | ThreeOfAKind of ThreeOfAKind
    | TwoPair of TwoPair
    | Pair of Pair
    | HighCard of HighCard

let parseCard (s: string): Card =
    let (value: Value) =
        match s.[0] with
        | 'A' -> Ace
        | 'K' -> King
        | 'Q' -> Queen
        | 'J' -> Jack
        | 'T' -> Ten
        | '2' -> Two
        | '3' -> Three
        | '4' -> Four
        | '5' -> Five
        | '6' -> Six
        | '7' -> Seven
        | '8' -> Eight
        | '9' -> Nine

    let suite =
        match s.[1] with
        | 'H' -> Hearts
        | 'S' -> Spades
        | 'D' -> Diamonds
        | 'C' -> Clubs
        | _ -> failwith "Unrecognized suite"

    { Value = value; Suite = suite }

let formatCard (c: Card) = $"<%A{c.Value} of %A{c.Suite}>"

let parseHand (h: string) =
    let hand =
        h.Split ' '
        |> Seq.map parseCard
        |> Seq.sortBy (fun c -> c.Value)
        |> Seq.rev

    hand |> Seq.iter (formatCard >> printfn "%s")
