open Typing

let is_none m =
  match m with
  | None -> true
  | Some _ -> false

(* testing matrix multiplication *)
let matmultyp = [Nparray ["A"; "B"]; Nparray ["B"; "C"]], Nparray ["A"; "C"]

let mat1typ = Nparray ["X"; "Y"]
let mat2typ = Nparray ["Y"; "Z"]
let result_typ = check_app matmultyp [mat1typ; mat2typ]
let _ = assert (result_typ = Some (Nparray ["X"; "Z"]))

(* testing bad matrix multiplication *)
let result_typ2 = check_app matmultyp [Nparray ["X"; "Z"]; Nparray ["Y"; "Z"]]
let _ = assert (result_typ2 = None)

(* testing good determinant of a square *)
let determinant = [Nparray ["A"; "A"]], Nparray []
let square = check_app determinant [Nparray ["X"; "X"]]
let _ = assert (square = Some (Nparray []))

(* testing bad determinant *)
let not_square = check_app determinant [Nparray ["X"; "Y"]]
let _ = assert (not_square = None)
