open Typing

let is_none m =
  match m with
  | None -> true
  | Some _ -> false

(* testing matrix multiplication *)
let _ =
  let a, b, c, x, y, z = Z3utils.(mk_string (), mk_string (), mk_string (),
                                  mk_string (), mk_string (), mk_string ()) in
  let matmultyp = ["A", Nparray [Id a; Id b]; "B", Nparray [Id b; Id c]], Nparray [Id a; Id c] in

  (* good matrix multiplication *)
  let result_typ = check_app matmultyp [[x; y]; [y; z]] in
  let _ =
    match result_typ with
    | [x'; z'] ->
        assert (Z3utils.prove_int_eq (Z3utils.mk_int x) (Z3utils.mk_int x')) ;
        assert (Z3utils.prove_int_eq (Z3utils.mk_int z) (Z3utils.mk_int z'))
    | _ -> assert false in

  (* testing bad matrix multiplication *)
  let _ =
    try
      let _result_typ = check_app matmultyp [[x; z]; [y; z]] in
      assert false
    with TypeError _ -> assert true in
  ()


(* testing determinant signature *)
let _ =
  let a, x, y = Z3utils.(mk_string (), mk_string (), mk_string()) in

  (* testing good determinant of a square *)
  let determinant = ["X", Nparray [Id a; Id a]], Nparray [] in
  let square = check_app determinant [[x; x]] in
  let _ = assert (square = []) in

  (* testing bad determinant *)
  try
    let _not_square = check_app determinant [[x; y]] in
    assert false
  with TypeError _ -> assert true

let assert_bad_kind (f : funtyp) =
  try
    let _ = check_app f [] in
    assert false
  with KindError s -> assert true

let _ =
  let a, b, c, d = Z3utils.(mk_string (), mk_string (),mk_string (),mk_string ()) in

  (* testing bad type declaration (intros variable within Add) *)
  let bad_typ = ["X", Nparray [Id a; Add (Id a, Id b)]], Nparray [] in
  let _ = assert_bad_kind bad_typ in

  (* testing bad type declaration (intros variable in return) *)
  let bad_typ = [], Nparray [Id a] in
  let _ = assert_bad_kind bad_typ in

  (* testing bad type declaration (intros spread variable in return) *)
  let bad_typ = [], Nparray [Spread a] in
  let _ = assert_bad_kind bad_typ in

  (* testing bad type declaration (intros variable in Mul) *)
  let bad_typ = ["X", Nparray [Mul (Id a, Id b)]], Nparray [] in
  let _ = assert_bad_kind bad_typ in

  (* testing bad type declaration (spread variable shadows variable) *)
  let bad_typ = ["X", Nparray [Id a; Spread a]], Nparray [] in
  let _ = assert_bad_kind bad_typ in

  (* testing bad type declaration (variable shadows spread variable) *)
  let bad_typ = ["X", Nparray [Spread a; Id a]], Nparray [] in
  let _ = assert_bad_kind bad_typ in

  (* testing bad type declaration (reuse param name) *)
  let bad_typ = ["X", Nparray []; "X", Nparray []], Nparray [] in
  let _ = assert_bad_kind bad_typ in

  (* testing bad type declaration (param_name would shadow spread) *)
  let bad_typ = ["X", Nparray [Spread a]; a, Nparray []], Nparray[] in
  let _ = assert_bad_kind bad_typ in

  (* testing bad type declaration (param_name shadow id) *)
  let bad_typ = ["X", Nparray [Id a]; a, Nparray []], Nparray [] in
  let _ = assert_bad_kind bad_typ in

  (* testing bad type declaration (id shadows param name) *)
  let bad_typ = [a, Nparray [Id a]], Nparray [] in
  let _ = assert_bad_kind bad_typ in

  (* testing bad type declaration (spread shadows param name) *)
  let bad_typ = [a, Nparray [Spread a]], Nparray [] in
  let _ = assert_bad_kind bad_typ in

  (* testing bad type declaration (spread inside Add) *)
  let bad_typ = ["X", Nparray [Spread a; Spread b; Add (Spread a, Spread b)]], Nparray [] in
  let _ = assert_bad_kind bad_typ in

  (* testing bad type declaration (spread inside Mul *)
  let bad_typ = ["X", Nparray [Spread a; Spread b; Mul (Spread a, Spread b)]], Nparray [] in
  let _ = assert_bad_kind bad_typ in
  ()

(* testing Add type declaration *)
let _ =
  let a, b, x, y = Z3utils.(mk_string (),mk_string (),mk_string (),mk_string ()) in
  let add_typ = ["A", Nparray [Id a; Id b]], Nparray [Add (Id a, Id b)] in
  match check_app add_typ [[x; y]] with
  | [x'] ->
      assert (Z3utils.prove_int_eq (Z3utils.mk_int x') (Z3utils.add_int [x; y]))
  | _ -> assert false

(* testing nested Add type declaration *)
let _ =
  let a, b, c, x, y, z = Z3utils.(mk_string (), mk_string (),mk_string (),mk_string (),mk_string (),mk_string ()) in
  let nested_Add_typ = ["X", Nparray [Id a; Id b; Id c]], Nparray [Add (Id a, Add (Id b, Id c))] in
  match check_app nested_Add_typ [[x; y; z]] with
  | [x'] ->
      assert (Z3utils.prove_int_eq (Z3utils.mk_int x') (Z3utils.add_int [x; y; z]))
  | _ -> assert false

(* testing Mul typ declaration *)
let _ =
  let a, b, x, y = Z3utils.(mk_string (), mk_string (),mk_string (),mk_string ()) in
  let mul_typ = ["X", Nparray [Id a; Id b]], Nparray [Mul (Id a, Id b)] in
  match check_app mul_typ [[x; y]] with
  | [x'] ->
      assert (Z3utils.prove_int_eq (Z3utils.mk_int x') (Z3utils.mul_int [x; y]))
  | _ -> assert false

(* testing Spread typ declaration *)
let _ =
  let a, b, c, x, y, z = Z3utils.(mk_string (), mk_string (), mk_string (),
                                  mk_string (), mk_string (), mk_string ()) in

  (* most basic spread test *)
  let spread_typ = ["X", Nparray [Spread a]], Nparray [Spread a] in
  let _ =
    match check_app spread_typ [[x; y]] with
    | [x'; y'] ->
        assert (Z3utils.prove_int_eq (Z3utils.mk_int x) (Z3utils.mk_int x')) ;
        assert (Z3utils.prove_int_eq (Z3utils.mk_int y) (Z3utils.mk_int y'))
    | _ -> assert false in

  let _ =
    match check_app spread_typ [[x; y; z]] with
    | [x'; y'; z'] ->
        assert (Z3utils.prove_int_eq (Z3utils.mk_int x) (Z3utils.mk_int x')) ;
        assert (Z3utils.prove_int_eq (Z3utils.mk_int y) (Z3utils.mk_int y')) ;
        assert (Z3utils.prove_int_eq (Z3utils.mk_int z) (Z3utils.mk_int z'))
    | _ -> assert false in

  (* check reuse of spread variable *)
  let spread_typ = ["X", Nparray [Spread a]; "Y", Nparray [Spread a]], Nparray [Spread a] in
  let _ =
    match check_app spread_typ [[x; y]; [x; y]] with
    | [x'; y'] ->
        assert (Z3utils.prove_int_eq (Z3utils.mk_int x) (Z3utils.mk_int x')) ;
        assert (Z3utils.prove_int_eq (Z3utils.mk_int y) (Z3utils.mk_int y'))

    | _ -> assert false in

  let spread_typ = ["X", Nparray [Spread a; Id b]; "Y", Nparray [Id b]], Nparray [Id b; Spread a] in
  let _ =
    match check_app spread_typ [[x; y]; [y]] with
    | [y'; x'] ->
        assert (Z3utils.prove_int_eq (Z3utils.mk_int x) (Z3utils.mk_int x')) ;
        assert (Z3utils.prove_int_eq (Z3utils.mk_int y) (Z3utils.mk_int y'))

    | _ -> assert false in

  let spread_type = ["X", Nparray [Spread a; Id b; Spread c]; "Y", Nparray [Spread a; Spread c]], Nparray [Spread c] in
  let _ =
    match check_app spread_type [[x; y; z]; [x; z]] with
    | [z'] ->
        assert (Z3utils.prove_int_eq (Z3utils.mk_int z') (Z3utils.mk_int z))

    | _ -> assert false in

  ()
