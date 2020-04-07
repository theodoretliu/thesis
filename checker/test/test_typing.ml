open Numpy_checking.Typing
open Numpy_checking.Z3utils

let is_none m =
  match m with
  | None -> true
  | Some _ -> false

(* testing matrix multiplication *)
let _ =
  let a, b, c, x, y, z = (mk_string (), mk_string (), mk_string (),
                                  mk_string (), mk_string (), mk_string ()) in
  let matmultyp = ["A", Nparray [Id a; Id b]; "B", Nparray [Id b; Id c]], Nparray [Id a; Id c] in

  (* good matrix multiplication *)
  let result_typ = check_app matmultyp [Dimensions [x; y]; Dimensions [y; z]] in
  let _ =
    match result_typ with
    | Dimensions [x'; z'] ->
        assert (prove_int_eq (mk_int x) (mk_int x')) ;
        assert (prove_int_eq (mk_int z) (mk_int z'))
    | _ -> assert false in

  (* testing bad matrix multiplication *)
  let _ =
    try
      let _result_typ = check_app matmultyp [Dimensions [x; z]; Dimensions [y; z]] in
      assert false
    with TypeError _ -> assert true in
  ()


(* testing determinant signature *)
let _ =
  let a, x, y = (mk_string (), mk_string (), mk_string()) in

  (* testing good determinant of a square *)
  let determinant = ["X", Nparray [Id a; Id a]], Nparray [] in
  let square = check_app determinant [Dimensions [x; x]] in
  let _ = assert (square = Dimensions []) in

  (* testing bad determinant *)
  try
    let _not_square = check_app determinant [Dimensions [x; y]] in
    assert false
  with TypeError _ -> assert true

let assert_bad_kind (f : funtyp) =
  try
    let _ = check_app f [] in
    assert false
  with KindError _s -> assert true

let _ =
  let a, b, c, d = (mk_string (), mk_string (),mk_string (),mk_string ()) in

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

  (* drop first arg is not spread *)
  let bad_typ = ["x", Nparray [Id a]], Nparray [Drop (a, [])] in
  let _ = assert_bad_kind bad_typ in

  (* drop first arg is not spread *)
  let bad_typ = ["x", Nparray [Id a]], Nparray [Drop ("x", [])] in
  let _ = assert_bad_kind bad_typ in

  (* drop second arg not int list *)
  let bad_typ = [a, Nparray [Spread b]; c, Nparray [Id d]], Nparray [Drop (b, [c])] in
  let _ = assert_bad_kind bad_typ in

  (* keep first arg is not spread *)
  let bad_typ = ["x", Nparray [Id a]], Nparray [Keep (a, [])] in
  let _ = assert_bad_kind bad_typ in

  (* keep first arg is not spread *)
  let bad_typ = ["x", Nparray [Id a]], Nparray [Keep ("x", [])] in
  let _ = assert_bad_kind bad_typ in

  (* keep second arg not int list *)
  let bad_typ = [a, Nparray [Spread b]; c, Nparray [Id d]], Nparray [Keep (b, [c])] in
  let _ = assert_bad_kind bad_typ in

  ()

(* testing Add type declaration *)
let _ =
  let a, b, x, y = (mk_string (),mk_string (),mk_string (),mk_string ()) in
  let add_typ = ["A", Nparray [Id a; Id b]], Nparray [Add (Id a, Id b)] in
  match check_app add_typ [Dimensions [x; y]] with
  | Dimensions [x'] ->
      assert (prove_int_eq (mk_int x') (add_int [x; y]))
  | _ -> assert false

(* testing nested Add type declaration *)
let _ =
  let a, b, c, x, y, z = (mk_string (), mk_string (),mk_string (),mk_string (),mk_string (),mk_string ()) in
  let nested_Add_typ = ["X", Nparray [Id a; Id b; Id c]], Nparray [Add (Id a, Add (Id b, Id c))] in
  match check_app nested_Add_typ [Dimensions [x; y; z]] with
  | Dimensions [x'] ->
      assert (prove_int_eq (mk_int x') (add_int [x; y; z]))
  | _ -> assert false

(* testing Mul typ declaration *)
let _ =
  let a, b, x, y = (mk_string (), mk_string (),mk_string (),mk_string ()) in
  let mul_typ = ["X", Nparray [Id a; Id b]], Nparray [Mul (Id a, Id b)] in
  match check_app mul_typ [Dimensions [x; y]] with
  | Dimensions [x'] ->
      assert (prove_int_eq (mk_int x') (mul_int [x; y]))
  | _ -> assert false

(* testing Spread typ declaration *)
let _ =
  let a, b, c, x, y, z = (mk_string (), mk_string (), mk_string (),
                                  mk_string (), mk_string (), mk_string ()) in

  (* most basic spread test *)
  let spread_typ = ["X", Nparray [Spread a]], Nparray [Spread a] in
  let _ =
    match check_app spread_typ [Dimensions [x; y]] with
    | Dimensions [x'; y'] ->
        assert (prove_int_eq (mk_int x) (mk_int x')) ;
        assert (prove_int_eq (mk_int y) (mk_int y'))
    | _ -> assert false in

  let _ =
    match check_app spread_typ [Dimensions [x; y; z]] with
    | Dimensions [x'; y'; z'] ->
        assert (prove_int_eq (mk_int x) (mk_int x')) ;
        assert (prove_int_eq (mk_int y) (mk_int y')) ;
        assert (prove_int_eq (mk_int z) (mk_int z'))
    | _ -> assert false in

  (* check reuse of spread variable *)
  let spread_typ = ["X", Nparray [Spread a]; "Y", Nparray [Spread a]], Nparray [Spread a] in
  let _ =
    match check_app spread_typ [Dimensions [x; y]; Dimensions [x; y]] with
    | Dimensions [x'; y'] ->
        assert (prove_int_eq (mk_int x) (mk_int x')) ;
        assert (prove_int_eq (mk_int y) (mk_int y'))

    | _ -> assert false in

  let spread_typ = ["X", Nparray [Spread a; Id b]; "Y", Nparray [Id b]], Nparray [Id b; Spread a] in
  let _ =
    match check_app spread_typ [Dimensions [x; y]; Dimensions [y]] with
    | Dimensions [y'; x'] ->
        assert (prove_int_eq (mk_int x) (mk_int x')) ;
        assert (prove_int_eq (mk_int y) (mk_int y'))

    | _ -> assert false in

  let spread_type = ["X", Nparray [Spread a; Id b; Spread c]; "Y", Nparray [Spread a; Spread c]], Nparray [Spread c] in
  let _ =
    match check_app spread_type [Dimensions [x; y; z]; Dimensions [x; z]] with
    | Dimensions [z'] ->
        assert (prove_int_eq (mk_int z') (mk_int z))

    | _ -> assert false in

  ()


(* testing int typing *)
let _ =
  let a, b, c, d, e, f = (mk_string(), mk_string(), mk_string(),
                          mk_string(), mk_string(), mk_string()) in
  let int_typ = [a, TypeInt], TypeInt in
  let _ = assert (check_app int_typ [Int] = Int) in

  let _ = assert (check_app int_typ [LiteralInt 4] = Int) in

  (* check that ints can be interpreted as empty array *)
  let nparray = [a, Nparray[]], TypeInt in
  let _ = assert (check_app nparray [Int] = Int) in

  let _ = assert (check_app nparray [LiteralInt 5] = Int) in

  (* check that empty array can be interpreted as int *)
  let final_typ = [a, TypeInt], TypeInt in
  let _ = assert (check_app final_typ [Dimensions []] = Int) in

  let _ = assert (check_app final_typ [Dimensions []] = Int) in

  ()

(* testing drop typing *)
let _ =
  let a, b, c, d, e, f, g = (mk_string(), mk_string(), mk_string(), mk_string(),
                             mk_string(), mk_string(), mk_string()) in

  (* testing drop of a single axis *)
  let drop_typ = [a, Nparray [Spread b]; c, TypeInt], Nparray [Drop (b, [c])] in
  let _ =
    match check_app drop_typ [Dimensions [d; e; f; g]; LiteralInt 2] with
    | Dimensions [d'; e'; g'] ->
        List.combine [d'; e'; g'] [d; e; g] |>
          List.iter (fun (x, y) -> assert (prove_int_eq (mk_int x) (mk_int y)))
    | _ -> assert false in

  (* testing drop where index exceeds provided length *)
  let _ =
    try
      let _ = check_app drop_typ [Dimensions [d; e; f]; LiteralInt 3] in
      assert false
    with TypeError _ ->
      assert true in

  (* testing weird drop *)
  let drop_typ = [a, Nparray [Spread b]; c, Nparray []], Nparray [Drop (b, [c])] in
  let _ =
    match check_app drop_typ [Dimensions [d; e; f; g]; LiteralInt 3] with
    | Dimensions [d'; e'; f'] ->
        List.combine [d'; e'; f'] [d; e; f] |>
          List.iter (fun (x, y) -> assert (prove_int_eq (mk_int x) (mk_int y)))
    | _ -> assert false in

  (* testing drop of multiple indices *)
  let drop_typ = [a, Nparray [Spread b]; c, TypeInt; d, TypeInt], Nparray [Drop (b, [c; d])] in
  let _ =
    match check_app drop_typ [Dimensions [d; e; f; g]; LiteralInt 0; LiteralInt 1] with
    | Dimensions [f'; g'] ->
        List.combine [f'; g'] [f; g] |>
          List.iter (fun (x, y) -> assert (prove_int_eq (mk_int x) (mk_int y)))
    | _ -> assert false in

  (* testing drop within the arguments *)
  let drop_typ = [a, Nparray [Spread b]; c, TypeInt; d, Nparray [Drop (b, [c])]], Nparray [Drop (b, [c])] in
  let _ =
    match check_app drop_typ [Dimensions [d; e; f; g]; LiteralInt 0; Dimensions [e; f; g]] with
    | Dimensions [e'; f'; g'] ->
        List.combine [e'; f'; g'] [e; f; g] |>
          List.iter (fun (x, y) -> assert (prove_int_eq (mk_int x) (mk_int y)))
    | _ -> assert false in

  (* test sort of advanced dropping leading into another drop *)
  let drop_typ' = [a, Nparray [Spread b]; c, TypeInt], Nparray [Drop (b, [c])] in
  let _ =
    let res = check_app drop_typ' [Dimensions [d; e; f; g]; LiteralInt 0] in
      match check_app drop_typ [Dimensions [d; e; f; g]; LiteralInt 0; res] with
      | Dimensions [e'; f'; g'] ->
          List.combine [e'; f'; g'] [e; f; g] |>
            List.iter (fun (x, y) -> assert (prove_int_eq (mk_int x) (mk_int y))) ;

      | _ -> assert false in

  (* dropping something that out of bounds *)
  let drop_typ_simp = [a, Nparray [Spread b]; c, TypeInt], Nparray [Drop (b, [c])] in
  let _ =
    try
      let _ = check_app drop_typ_simp [Dimensions [d; e; f]; LiteralInt 3] in
      assert false
    with TypeError _ -> assert true
  in

  (* trying to drop when we don't have further type information *)
  let _ =
    try
      let _ = check_app drop_typ_simp [Dimensions [d; e; f]; Int] in
      assert false
    with TypeError _ -> assert true
  in

  ()

(* testing keep typing *)
let _ =
  let a, b, c, d, e, f, g = (mk_string(), mk_string(), mk_string(), mk_string(),
                             mk_string(), mk_string(), mk_string()) in

  (* testing drop of a single axis *)
  let keep_typ = [a, Nparray [Spread b]; c, TypeInt], Nparray [Keep (b, [c])] in
  let _ =
    match check_app keep_typ [Dimensions [d; e; f; g]; LiteralInt 2] with
    | Dimensions [f'] ->
        assert (prove_int_eq (mk_int f) (mk_int f'))
    | _ -> assert false in

  (* testing keep where index exceeds provided length *)
  let _ =
    try
      let _ = check_app keep_typ [Dimensions [d; e; f]; LiteralInt 3] in
      assert false
    with TypeError _ ->
      assert true in

  (* testing weird keep *)
  let keep_typ = [a, Nparray [Spread b]; c, Nparray []], Nparray [Keep (b, [c])] in
  let _ =
    match check_app keep_typ [Dimensions [d; e; f; g]; LiteralInt 3] with
    | Dimensions [g'] ->
        List.combine [g] [g'] |>
          List.iter (fun (x, y) -> assert (prove_int_eq (mk_int x) (mk_int y)))
    | _ -> assert false in

  (* testing keep of multiple indices *)
  let keep_typ = [a, Nparray [Spread b]; c, TypeInt; d, TypeInt], Nparray [Keep (b, [c; d])] in
  let _ =
    match check_app keep_typ [Dimensions [d; e; f; g]; LiteralInt 0; LiteralInt 1] with
    | Dimensions [d'; e'] ->
        List.combine [d'; e'] [d; e] |>
          List.iter (fun (x, y) -> assert (prove_int_eq (mk_int x) (mk_int y)))
    | _ -> assert false in

  (* testing keep within the arguments *)
  let keep_typ = [a, Nparray [Spread b]; c, TypeInt; d, Nparray [Keep (b, [c])]], Nparray [Keep (b, [c])] in
  let _ =
    match check_app keep_typ [Dimensions [d; e; f; g]; LiteralInt 0; Dimensions [d]] with
    | Dimensions [d'] ->
        assert (prove_int_eq (mk_int d') (mk_int d))
    | _ -> assert false in

  (* test sort of advanced keeping leading into another keep *)
  let keep_typ' = [a, Nparray [Spread b]; c, TypeInt], Nparray [Keep (b, [c])] in
  let _ =
    let res = check_app keep_typ' [Dimensions [d; e; f; g]; LiteralInt 0] in
      match check_app keep_typ [Dimensions [d; e; f; g]; LiteralInt 0; res] with
      | Dimensions [d'] ->
          assert (prove_int_eq (mk_int d) (mk_int d'))

      | _ -> assert false in

  (* keepping something that out of bounds *)
  let keep_typ_simp = [a, Nparray [Spread b]; c, TypeInt], Nparray [Keep (b, [c])] in
  let _ =
    try
      let _ = check_app keep_typ_simp [Dimensions [d; e; f]; LiteralInt 3] in
      assert false
    with TypeError _ -> assert true
  in

  (* trying to keep when we don't have further type information *)
  let _ =
    try
      let _ = check_app keep_typ_simp [Dimensions [d; e; f]; Int] in
      assert false
    with TypeError _ -> assert true
  in

  ()
