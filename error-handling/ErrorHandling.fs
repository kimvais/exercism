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
    match twoTrackInput with
    | Ok s -> switchFunction s
    | Error e -> Error e

let cleanupDisposablesWhenThrowingException resource =
    use x = resource
    failwith (x.ToString())
