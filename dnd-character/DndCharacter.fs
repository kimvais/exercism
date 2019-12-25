module DndCharacter

open System

let rnd = System.Random()

let rollDice =
    rnd.Next(1, 6)

let infRolls =
    Seq.initInfinite (fun _ -> rollDice)

let rollNd6 n =
    infRolls |> Seq.take n

let roll4d6 = rollNd6 4


let modifier x =
    let i = float x 
    (i - 10.0) / 2.0 |> floor |> int

let ability() =
    let rolls = roll4d6
    Seq.sum rolls - Seq.min rolls

type DndCharacter() =
    member __.Strength = ability()
    member __.Dexterity = ability()
    member __.Constitution = ability()
    member __.Intelligence = ability()
    member __.Wisdom = ability()
    member __.Charisma = ability()
    member __.Hitpoints = 10 + modifier __.Constitution 
