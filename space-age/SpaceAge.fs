module SpaceAge

let year = 365.25 * 86400.0

type Planet =
    | Earth
    | Mercury
    | Venus
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune


let age (planet: Planet) (seconds: int64): float =
    let secs = float seconds
    match planet with
    | Earth -> secs / year
    | Mercury -> secs / (0.2408467 * year)
    | Venus -> secs / (0.61519726 * year)
    | Mars -> secs / (1.8808158 * year)
    | Jupiter -> secs / (11.862615 * year)
    | Saturn -> secs / (29.447498 * year)
    | Uranus -> secs / (84.016846 * year)
    | Neptune -> secs / (164.79132 * year)
