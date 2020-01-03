module ErrorHandling

let handleErrorByThrowingException() = failwith "Error"

let handleErrorByReturningOption (input: string) =
    try
        (int >> Some) input
    with _ -> None

let handleErrorByReturningResult (input: string) =
    try
        Ok(int input)
    with _ -> Error "Could not convert input to integer"

let bind switchFunction twoTrackInput =
    twoTrackInput |> Result.bind switchFunction

let cleanupDisposablesWhenThrowingException resource =
    use x = resource
    failwith (x.ToString())
