module BinarySearch

let find (input: 'a array) value =
    let rec search value start end' =
        if start > end' then
            None
        else
            let middle = (start + end') / 2
            match input.[middle] with
            | s when s = value -> Some middle
            | s when s > value -> search value start (middle - 1)
            | s when s < value -> search value (middle + 1) end'
    search value 0 (Array.length input - 1)
