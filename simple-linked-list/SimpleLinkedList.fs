module SimpleLinkedList


type LinkedList<'a> =
    | Nil
    | Node of datum: 'a * next: LinkedList<'a>


let nil = Nil

let create x n =
    Node(x, n)


let next: LinkedList<'a> -> LinkedList<'a> =
    function
    | Nil -> Nil
    | Node(_, n) -> n

let datum: LinkedList<'a> -> 'a =
    function
    | Nil -> failwith "No datum for nil"
    | Node(d, _) -> d

let isNil: LinkedList<'a> -> bool =
    function
    | Nil -> true
    | _ -> false

let toSeq ll =
    let mutable node = ll
    seq {
        while not (isNil node) do
            yield datum node
            node <- next node
    }

let toList x = toSeq x |> Seq.toList

let fromSeq s =
    Seq.foldBack create s Nil

let fromList xs = List.toSeq xs |> fromSeq

let reverse x =
    toSeq x
    |> Seq.rev
    |> fromSeq
