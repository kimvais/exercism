module RobotSimulator

type Direction =
    | North
    | East
    | South
    | West

type Position = int * int

type Robot =
    { direction: Direction
      position: Position }

let turn direction robot =
    { robot with direction = direction }

let turnLeft r =
    match r.direction with
    | North -> turn West r
    | West -> turn South r
    | South -> turn East r
    | East -> turn North r

let turnRight r =
    match r.direction with
    | North -> turn East r
    | East -> turn South r
    | South -> turn West r
    | West -> turn North r

let advance robot =
    let d = robot.direction
    let (x, y) = robot.position
    match d with
    | North ->
        { robot with position = (x, y + 1) }
    | East ->
        { robot with position = (x + 1, y) }
    | South ->
        { robot with position = (x, y - 1) }
    | West ->
        { robot with position = (x - 1, y) }

let doStep robot c =
    match c with
    | 'L' -> turnLeft robot
    | 'R' -> turnRight robot
    | 'A' -> advance robot
    | _ -> failwith "Invalid instruction"

let create direction position =
    { direction = direction
      position = position }

let move instructions robot =
    instructions |> Seq.fold doStep robot
