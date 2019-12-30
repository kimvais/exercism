module Triangle

let istriangle sides =
    (sides |> Seq.sum) > (2.0 * (sides |> Seq.max))

let evaluate f triangle = 
    istriangle triangle && (f <| (triangle
                             |> Seq.distinct
                             |> Seq.length))
    
let equilateral triangle =
    evaluate ((=) 1) triangle

let isosceles triangle =
    evaluate ((>) 3) triangle

let scalene triangle =
    evaluate ((=) 3) triangle
