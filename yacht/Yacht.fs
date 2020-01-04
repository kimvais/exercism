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

let isValid (throws: Die list) =
    List.length throws = 5

let countDice dice =
    List.groupBy (id) dice |> List.map (fun (i, s) -> (List.length s, i))

let getDiceWithCount count dice =
    countDice dice |> List.filter (fun d -> fst d = count)

let scoreNOfKind n dice =
    let setsOfN = countDice dice |> List.filter (fun (c, _) -> c >= n)
    match setsOfN with
    | s when List.isEmpty s -> 0
    | s ->
        s
        |> List.maxBy fst
        |> (fun (_, d) -> n * getScore d)

let scoreNumbers cat =
    List.filter ((=) cat)
    >> List.length
    >> (*) (getScore cat)

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
    let diceCounts = countDice dice
    match countDice dice
          |> List.map fst
          |> Set.ofList = set [ 2; 3 ] with
    | true -> diceCounts |> List.sumBy (fun (c, d) -> c * getScore d)
    | false -> 0

let score category dice =
    match category with
    | Yacht -> scoreYacht dice
    | Ones -> (scoreNumbers One) dice
    | Twos -> (scoreNumbers Two) dice
    | Threes -> (scoreNumbers Three) dice
    | Fours -> (scoreNumbers Four) dice
    | Fives -> (scoreNumbers Five) dice
    | Sixes -> (scoreNumbers Six) dice
    | Choice -> dice |> List.sumBy getScore
    | LittleStraight -> (scoreStraight [ 1 .. 5 ]) dice
    | BigStraight -> (scoreStraight [ 2 .. 6 ]) dice
    | FourOfAKind -> scoreNOfKind 4 dice
    | FullHouse -> scoreFullHouse dice
