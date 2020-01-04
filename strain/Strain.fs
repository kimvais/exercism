module Seq

let public keep pred xs =
    seq {
        for x in xs do
            if pred x then yield x
    }

let public discard pred =
    keep (pred >> not)
