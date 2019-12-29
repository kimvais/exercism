module RobotName

let rnd = System.Random()
let letters = [ 'A' .. 'Z' ]

type Robot =
    { id: int }

let mkRobot() =
    { id = rnd.Next(1, 26 * 26 * 1000) }

let name robot =
    let a = letters.[robot.id / 1000 % 26]
    let b = letters.[robot.id / 26000 % 26]
    let n = robot.id % 1000
    sprintf "%c%c%03d" a b n

let reset robot =
    mkRobot()
