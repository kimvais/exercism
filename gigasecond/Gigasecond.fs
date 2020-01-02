module Gigasecond

open System

let add (dt: DateTime) = dt.Add(TimeSpan(0, 0, 1_000_000_000))
