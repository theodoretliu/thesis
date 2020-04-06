open Numpy_checking.Z3utils

let x = mk_int "x"
let other_x = mk_int "x"
let _ = assert (prove_int_eq x other_x)

let y = mk_int "y"
let _ = assert (not (prove_int_eq x y))
