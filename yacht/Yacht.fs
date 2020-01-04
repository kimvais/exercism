module Yacht

type Category =
    | Ones
    | Twos
    | Threes
    | Fours
    | Fives
    | Sixes
    | FullHouse
    | FourOfAKind
    | LittleStraight
    | BigStraight
    | Choice
    | Yacht

type Die =
    | One
    | Two
    | Three
    | Four
    | Five
    | Six

let getScore =
    function
    | One -> 1
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6

// This isn't part of test suite
let isValid (dice: Die list) =
    match List.length dice = 5 with
    | true -> dice
    | false -> failwith "Need 5 dice"

let countDice dice =
    List.groupBy (id) dice |> List.map (fun (i, s) -> (List.length s, i))

let scoreNOfKind n dice =
    let setsOfN = countDice dice |> List.filter (fun (c, _) -> c >= n)
    match setsOfN with
    | s when List.isEmpty s -> 0
    | _ ->
        setsOfN
        |> List.maxBy fst
        |> (fun (_, d) -> n * getScore d)

let scoreNumbers category =
    List.filter ((=) category) >> List.sumBy getScore

let scoreYacht dice =
    match countDice dice |> List.tryExactlyOne with
    | Some _ -> 50
    | None -> 0

let scoreStraight range dice =
    match (dice
           |> Set.ofList
           |> Set.map getScore = Set.ofList range) with
    | true -> 30
    | false -> 0

let scoreFullHouse dice =
    match countDice dice
          |> List.map fst
          |> Set.ofList = set [ 2; 3 ] with
    | true ->
        dice
        |> List.map getScore
        |> List.sum
    | false -> 0

let score category =
    isValid
    >> match category with
       | Yacht -> scoreYacht
       | Ones -> scoreNumbers One
       | Twos -> scoreNumbers Two
       | Threes -> scoreNumbers Three
       | Fours -> scoreNumbers Four
       | Fives -> scoreNumbers Five
       | Sixes -> scoreNumbers Six
       | Choice -> List.sumBy getScore
       | LittleStraight -> scoreStraight [ 1 .. 5 ]
       | BigStraight -> scoreStraight [ 2 .. 6 ]
       | FourOfAKind -> scoreNOfKind 4
       | FullHouse -> scoreFullHouse
