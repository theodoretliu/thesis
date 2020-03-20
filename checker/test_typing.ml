open Typing

let is_none m =
  match m with
  | None -> true
  | Some _ -> false

(* testing matrix multiplication *)
let matmultyp = [Nparray [Id "A"; Id "B"]; Nparray [Id "B"; Id "C"]], Nparray [Id "A"; Id "C"]

let mat1typ = Nparray [Id "X"; Id "Y"]
let mat2typ = Nparray [Id "Y"; Id "Z"]
let result_typ = check_app matmultyp [mat1typ; mat2typ]

let _ =
  match result_typ with
  | Some (Nparray [Id x; Id z]) ->
      assert (Z3utils.prove_int_eq (Z3utils.mk_int x) (Z3utils.mk_int "X")) ;
      assert (Z3utils.prove_int_eq (Z3utils.mk_int z) (Z3utils.mk_int "Z"))
  | _ -> assert false

(* testing bad matrix multiplication *)
let result_typ2 = check_app matmultyp [Nparray [Id "X"; Id "Z"]; Nparray [Id "Y"; Id "Z"]]
let _ = assert (result_typ2 = None)

(* testing good determinant of a square *)
let determinant = [Nparray [Id "A"; Id "A"]], Nparray []
let square = check_app determinant [Nparray [Id "X"; Id "X"]]
let _ = assert (square = Some (Nparray []))

(* testing bad determinant *)
let not_square = check_app determinant [Nparray [Id "X"; Id "Y"]]
let _ = assert (not_square = None)

(* testing bad type declaration *)
let bad_typ = [Nparray [Id "A"; Add (Id "A", Id "B")]], Nparray []
let bad_res = check_app bad_typ [Nparray [Id "X"; Add (Id "X", Id "Y")]]