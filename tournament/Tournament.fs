module Tournament

let header = [ "Team                           | MP |  W |  D |  L |  P" ]

let parseOutcome (row: string) =
    seq
        [ match row.Split ';' with
          | [| a; b; "win" |] ->
              yield a, (1, 1, 0, 0, 3)
              yield b, (1, 0, 0, 1, 0)
          | [| a; b; "draw" |] ->
              yield a, (1, 0, 1, 0, 1)
              yield b, (1, 0, 1, 0, 1)
          | [| a; b; "loss" |] ->
              yield b, (1, 1, 0, 0, 3)
              yield a, (1, 0, 0, 1, 0)
          | _ -> failwith (sprintf "Invalid result %s" row) ]

let scoreFolder (mp, w, d, l, p) (mp', w', d', l', p') = mp+mp', w + w', d + d', l + l', p + p'

let formatResults (name, (mp, wins, draws, losses, points)) =
    $"%-30s{name} | %2d{mp} | %2d{wins} | %2d{draws} | %2d{losses} | %2d{points}"
    
let tally input =
    let scores =
        input
        |> List.map parseOutcome
        |> Seq.concat
        |> Seq.groupBy fst
        |> Seq.map (fun (a, b) -> a, (Seq.map snd b))
    let results = 
        scores
        |> Seq.map (fun (a, b) -> a, Seq.fold scoreFolder (0, 0, 0, 0, 0) b)
        |> Seq.sortBy (fun (name, (_,_,_,_,points)) -> (-points, name))
        |> Seq.map formatResults
        |> List.ofSeq
    header @ results
