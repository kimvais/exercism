module Poker


type Suite =
    | Clubs
    | Diamonds
    | Spades
    | Hearts

type Value =
    | Deuce
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


type Card = Value * Suite

type Hand = Card array

type PokerHand =
    | HighCard
    | Pair
    | TwoPair
    | ThreeOfAKind
    | Straight
    | Flush
    | FullHouse
    | FourOfAKind
    | StraightFlush

let getValue card =
    fst card
    |> function
        | Ace -> 14
        | King -> 13
        | Queen -> 12
        | Jack -> 11
        | Ten -> 10
        | Nine -> 9
        | Eight -> 8
        | Seven -> 7
        | Six -> 6
        | Five -> 5
        | Four -> 4
        | Three -> 3
        | Deuce -> 2

let isFlush (h: Hand) = (h |> Array.map snd |> Set.ofSeq |> Set.count) = 1

let isStraight (h: Hand) =
    let values = h |> Array.map getValue

    Array.sort values
    |> Seq.pairwise
    |> Seq.forall (fun (a, b) -> b - a = 1)
    || (Array.sortDescending values = [| 14; 5; 4; 3; 2 |])

let isStraightFlush h = isStraight h && isFlush h

let hasGroups groups h =
    (h |> Array.countBy fst |> Array.map snd |> Array.sortDescending) = groups

let isFourOfAKind = hasGroups [| 4; 1 |]

let isFullHouse = hasGroups [| 3; 2 |]

let isThreeOfAKind = hasGroups [| 3; 1; 1 |]

let isTwoPair = hasGroups [| 2; 2; 1 |]

let isPair = hasGroups [| 2; 1; 1; 1 |]

let parseCard (s: string) : Card =
    let (value: Value) =
        match s.[0] with
        | 'A' -> Ace
        | 'K' -> King
        | 'Q' -> Queen
        | 'J' -> Jack
        | 'T' -> Ten
        | '2' -> Deuce
        | '3' -> Three
        | '4' -> Four
        | '5' -> Five
        | '6' -> Six
        | '7' -> Seven
        | '8' -> Eight
        | '9' -> Nine
        | _ -> failwith "Unrecognized value"

    let suite =
        match s.[1] with
        | 'H' -> Hearts
        | 'S' -> Spades
        | 'D' -> Diamonds
        | 'C' -> Clubs
        | _ -> failwith "Unrecognized suite"

    value, suite

let parseHand (h: string) : Hand =
    let hand =
        h.Replace("10", "T").Split ' '
        |> Seq.map parseCard
        |> Seq.sortBy fst
        |> Seq.rev

    hand |> Array.ofSeq

let getStraightValues h =
    h
    |> Array.map getValue
    |> Array.map (function
        | v when v = 14 -> 1
        | v -> v)
    |> Array.sortDescending

let getValues h =
    h
    |> Array.groupBy fst
    |> Array.sortByDescending (snd >> Array.length)
    |> Array.map snd
    |> Array.concat
    |> Array.map getValue

let rankHand =
    function
    | h when h |> isStraightFlush -> StraightFlush, getValues h
    | h when h |> isFourOfAKind -> FourOfAKind, getValues h
    | h when h |> isFullHouse -> FullHouse, getValues h
    | h when h |> isFlush -> Flush, getValues h
    | h when h |> isStraight -> Straight, getStraightValues h
    | h when h |> isThreeOfAKind -> ThreeOfAKind, getValues h
    | h when h |> isTwoPair -> TwoPair, getValues h
    | h when h |> isPair -> Pair, getValues h
    | h -> HighCard, getValues h

let bestHands hands =
    hands
    |> List.groupBy (parseHand >> rankHand)
    |> List.maxBy fst
    |> snd
