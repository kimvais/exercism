module GradeSchool

open System


type School = Map<int, string list>

let empty: School = Map.empty

let add (student: string) (grade: int) (school: School): School =
    match school.TryFind grade with
    | None -> school.Add(grade, [student])
    | Some l -> school.Add(grade, (l @ [student]))
    
let roster (school :School): string list =
    school |> (Map.toList >> List.map (snd >> List.sort) >> List.concat)

let grade (grade: int) (school: School): string list = 
    match school.TryFind grade with
    | None -> List.empty
    | Some l -> l |> List.sort
