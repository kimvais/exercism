module ReverseString

let reverse (input: string): string =
    Seq.rev input |> System.String.Concat
