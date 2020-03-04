open Typing

let is_none m =
  match m with
  | None -> true
  | Some _ -> false

let matmultyp = [Nparray ["A"; "B"]; Nparray ["B"; "C"]], Nparray ["A"; "C"]

let mat1typ = Nparray ["X"; "Y"]
let mat2typ = Nparray ["Y"; "Z"]

let result_typ = check_app matmultyp [mat1typ; mat2typ]

let _ =
  match result_typ with
  | None -> print_endline "None"
  | Some v -> print_typ v ; print_newline ()

let _ = assert (result_typ = Some (Nparray ["X"; "Z"]))

let result_typ2 = check_app matmultyp [Nparray ["X"; "Z"]; Nparray ["Y"; "Z"]]

let _ = assert (result_typ2 = None)
