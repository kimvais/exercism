module ListOps

let rec foldl = List.fold

let rec foldr folder state list = List.foldBack folder list state

let length = List.length

let reverse = List.rev

let map = List.map

let filter = List.filter

let append = List.append

let concat = List.concat
