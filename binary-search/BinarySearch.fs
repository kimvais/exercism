module BinarySearch

let rec search (input: 'a array) value start end' =
    let middle = (start + end') / 2
    if start < 0 || end' > Array.length input || start > end' then
        None
    else
        match input.[middle] with
        | s when s = value -> Some middle
        | s when s > value -> search input value start (middle - 1)
        | s when s < value -> search input value (middle + 1) end'
        | _ -> None

let find input value =
    search input value 0 (Array.length input - 1)
