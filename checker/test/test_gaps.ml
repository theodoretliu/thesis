open Numpy_checking.Typing
open Numpy_checking.Z3utils

(* tests for the type-system gap patches; see docs/README.md *)

let failures = ref 0

let expect name f =
  let fail msg =
    incr failures;
    print_endline ("FAIL " ^ name ^ msg)
  in
  match f () with
  | true -> ()
  | false -> fail ""
  | exception e -> fail (": " ^ Printexc.to_string e)

let () = at_exit (fun () -> if !failures > 0 then exit 1)
let dims n = List.init n (fun _ -> mk_string ())

let rejects ?(kind = false) f args =
  try
    ignore (check_app f args);
    false
  with
  | TypeError _ -> not kind
  | KindError _ -> kind

let dims_equal (r : arg) (expected : Z3.Expr.expr list) =
  match r with
  | Dimensions l ->
      List.length l = List.length expected
      && List.for_all2 (fun v e -> prove_int_eq (mk_int v) e) l expected
  | _ -> false

let v = mk_int
let n = mk_int_numeral

(* ---- step 0: bugs on main ---- *)

(* Int literal inside Add: Ndarray[a] -> Ndarray[a + 1] *)
let () =
  expect "int inside add" (fun () ->
      let x = mk_string () in
      match
        check_app
          ([ ("X", Nparray [ Id "a" ]) ], Nparray [ Add (Id "a", Int 1) ])
          [ Dimensions [ x ] ]
      with
      | Dimensions [ r ] ->
          prove_int_eq (mk_int r)
            (Z3.Arithmetic.mk_add ctx [ mk_int x; mk_int_numeral 1 ])
      | _ -> false)

(* nested spread under Add must be a kind error, not a runtime TypeError *)
let () =
  expect "nested spread in add is kind error" (fun () ->
      try
        ignore
          (check_app
             ( [ ("X", Nparray [ Spread "A"; Id "b" ]) ],
               Nparray [ Add (Id "b", Add (Id "b", Spread "A")) ] )
             [ Dimensions [ mk_string (); mk_string () ] ]);
        false
      with KindError _ -> true)

(* thesis: zeros(k: int) -> Ndarray[k] *)
let () =
  expect "param reference in return (thesis zeros)" (fun () ->
      match
        check_app ([ ("k", TypeInt) ], Nparray [ Id "k" ]) [ LiteralInt 3 ]
      with
      | Dimensions [ r ] -> prove_int_eq (mk_int r) (mk_int_numeral 3)
      | _ -> false)

(* dims are naturals: a + b = 0 should force a = 0 *)
let () =
  expect "dims are nonnegative" (fun () ->
      let x, y = (mk_string (), mk_string ()) in
      ignore
        (check_app
           ([ ("X", Nparray [ Id "a"; Id "b" ]) ], Nparray [])
           [ Dimensions [ x; y ] ]);
      prove (Z3.Arithmetic.mk_ge ctx (v x) (n 0)))

let () =
  expect "drop inside add is kind error" (fun () ->
      rejects ~kind:true
        ( [ ("X", Nparray [ Spread "A"; Id "b" ]); ("i", TypeInt) ],
          Nparray [ Add (Id "b", Drop ("A", [ Left "i" ])) ] )
        [ Dimensions (dims 2); LiteralInt 0 ])

let () =
  expect "negative dimension literal is kind error" (fun () ->
      rejects ~kind:true ([], Nparray [ Int (-1) ]) [])

(* take(n: int, x: Ndarray[n, d]) -- param reference in argument position *)
let () =
  let f =
    ([ ("n", TypeInt); ("X", Nparray [ Id "n"; Id "d" ]) ], Nparray [ Id "d" ])
  in
  let x = mk_int_var 3 and d = mk_string () in
  expect "param reference in argument position" (fun () ->
      dims_equal (check_app f [ LiteralInt 3; Dimensions [ x; d ] ]) [ v d ]);
  expect "param reference mismatch rejected" (fun () ->
      rejects f [ LiteralInt 4; Dimensions [ x; d ] ]);
  expect "param reference with unknown int rejected" (fun () ->
      rejects f [ Int; Dimensions [ x; d ] ])

(* zeros(k: int) with a non-literal k: result is some (fresh) dimension *)
let () =
  expect "unknown int param gives fresh dim" (fun () ->
      match check_app ([ ("k", TypeInt) ], Nparray [ Id "k" ]) [ Int ] with
      | Dimensions [ r ] -> not (prove_int_eq (v r) (n 0))
      | _ -> false)

(* array parameters can't be used as dimensions *)
let () =
  expect "array param as dim is kind error" (fun () ->
      rejects ~kind:true
        ([ ("X", Nparray [ Id "a" ]) ], Nparray [ Id "X" ])
        [ Dimensions (dims 1) ])

(* ---- step 1: type-level arithmetic ---- *)

(* conv2d(x: [n, c, h, w], k: [o, c, kh, kw]) -> [n, o, h - kh + 1, w - kw + 1] *)
let conv2d =
  ( [
      ("X", Nparray [ Id "n"; Id "c"; Id "h"; Id "w" ]);
      ("K", Nparray [ Id "o"; Id "c"; Id "kh"; Id "kw" ]);
    ],
    Nparray
      [
        Id "n";
        Id "o";
        Add (Sub (Id "h", Id "kh"), Int 1);
        Add (Sub (Id "w", Id "kw"), Int 1);
      ] )

let lits = List.map mk_int_var

let () =
  expect "conv2d concrete" (fun () ->
      dims_equal
        (check_app conv2d
           [
             Dimensions (lits [ 2; 3; 32; 32 ]);
             Dimensions (lits [ 8; 3; 5; 5 ]);
           ])
        [ n 2; n 8; n 28; n 28 ]);
  expect "conv2d kernel larger than image rejected" (fun () ->
      rejects conv2d
        [ Dimensions (lits [ 2; 3; 4; 4 ]); Dimensions (lits [ 8; 3; 6; 6 ]) ]);
  expect "conv2d unrelated symbolic sizes rejected (may be negative)" (fun () ->
      let b, c, h, w, o, kh, kw =
        ( mk_string (),
          mk_string (),
          mk_string (),
          mk_string (),
          mk_string (),
          mk_string (),
          mk_string () )
      in
      rejects conv2d [ Dimensions [ b; c; h; w ]; Dimensions [ o; c; kh; kw ] ]);
  expect "conv2d symbolic image, concrete kernel, known lower bound" (fun () ->
      let b, c, o = (mk_string (), mk_string (), mk_string ()) in
      (* an image whose side is 4 + something *)
      let s = mk_string () in
      assume_dim s;
      let h = add_to_solver (Z3.Arithmetic.mk_add ctx [ v s; n 4 ]) in
      dims_equal
        (check_app conv2d
           [
             Dimensions [ b; c; h; h ];
             Dimensions [ o; c; mk_int_var 3; mk_int_var 3 ];
           ])
        [
          v b;
          v o;
          Z3.Arithmetic.mk_add ctx [ v s; n 2 ];
          Z3.Arithmetic.mk_add ctx [ v s; n 2 ];
        ])

(* max_pool(x: [*B, h, w], s: int) -> [*B, h // s, w // s] *)
let pool =
  ( [ ("X", Nparray [ Spread "B"; Id "h"; Id "w" ]); ("s", TypeInt) ],
    Nparray [ Spread "B"; Div (Id "h", Id "s"); Div (Id "w", Id "s") ] )

let () =
  let b = mk_string () in
  expect "pool floors" (fun () ->
      dims_equal
        (check_app pool [ Dimensions (b :: lits [ 33; 32 ]); LiteralInt 2 ])
        [ v b; n 16; n 16 ]);
  expect "pool by zero rejected" (fun () ->
      rejects pool [ Dimensions (b :: lits [ 32; 32 ]); LiteralInt 0 ]);
  expect "pool by unknown stride gives fresh dims" (fun () ->
      match check_app pool [ Dimensions (b :: lits [ 32; 32 ]); Int ] with
      | Dimensions [ _; _; _ ] -> true
      | _ -> false)

(* subtraction and division in argument position *)
let () =
  let f =
    ( [
        ("X", Nparray [ Id "a" ]);
        ("Y", Nparray [ Sub (Id "a", Int 1); Div (Id "a", Int 2) ]);
      ],
      Nparray [] )
  in
  expect "sub/div in argument" (fun () ->
      check_app f [ Dimensions (lits [ 9 ]); Dimensions (lits [ 8; 4 ]) ]
      = Dimensions []);
  expect "sub/div in argument mismatch" (fun () ->
      rejects f [ Dimensions (lits [ 9 ]); Dimensions (lits [ 8; 5 ]) ])

(* ---- step 2: values in types ---- *)

(* shape0(x: [n, *R]) -> int{n};  zeros(k: int) -> [k];  size(x: [a, b]) -> int{a * b} *)
let shape0 = ([ ("X", Nparray [ Id "n"; Spread "R" ]) ], IntExpr (Id "n"))
let zeros = ([ ("k", TypeInt) ], Nparray [ Id "k" ])

let size2 =
  ([ ("X", Nparray [ Id "a"; Id "b" ]) ], IntExpr (Mul (Id "a", Id "b")))

let iadd = ([ ("i", TypeInt); ("j", TypeInt) ], IntExpr (Add (Id "i", Id "j")))
let isub = ([ ("i", TypeInt); ("j", TypeInt) ], IntExpr (Sub (Id "i", Id "j")))

(* scale(x: [n, d], w: [n]) -> [d] *)
let scale =
  ( [ ("X", Nparray [ Id "n"; Id "d" ]); ("W", Nparray [ Id "n" ]) ],
    Nparray [ Id "d" ] )

let () =
  let x = dims 2 in
  expect "x.shape[0] flows into zeros" (fun () ->
      let k = check_app shape0 [ Dimensions x ] in
      let w = check_app zeros [ k ] in
      dims_equal (check_app scale [ Dimensions x; w ]) [ v (List.nth x 1) ]);
  expect "zeros(x.shape[0] + 1) mismatches x" (fun () ->
      let k =
        check_app iadd [ check_app shape0 [ Dimensions x ]; LiteralInt 1 ]
      in
      rejects scale [ Dimensions x; check_app zeros [ k ] ]);
  expect "size(x) is a*b" (fun () ->
      match check_app size2 [ Dimensions x ] with
      | SymInt s -> prove_int_eq (v s) (Z3.Arithmetic.mk_mul ctx (List.map v x))
      | _ -> false)

(* sum(x: [*A], axis: int) -> [Drop(A, axis)] *)
let sum_axis =
  ( [ ("X", Nparray [ Spread "A" ]); ("axis", TypeInt) ],
    Nparray [ Drop ("A", [ Left "axis" ]) ] )

let () =
  let x = dims 3 in
  expect "computed axis 3 - 2 is statically known (thesis example)" (fun () ->
      let axis = check_app isub [ LiteralInt 3; LiteralInt 2 ] in
      dims_equal
        (check_app sum_axis [ Dimensions x; axis ])
        [ v (List.nth x 0); v (List.nth x 2) ]);
  expect "undetermined symbolic axis rejected" (fun () ->
      rejects sum_axis [ Dimensions x; SymInt (mk_string ()) ])

let () =
  expect "zeros of unconstrained symbolic int rejected (may be negative)"
    (fun () -> rejects zeros [ SymInt (mk_string ()) ]);
  expect "zeros of opaque Int still gives fresh dim" (fun () ->
      match check_app zeros [ Int ] with Dimensions [ _ ] -> true | _ -> false)

(* IntExpr in parameter position: at(x: [n], i: int{n - 1}) *)
let () =
  let f =
    ( [ ("X", Nparray [ Id "n" ]); ("i", IntExpr (Sub (Id "n", Int 1))) ],
      TypeInt )
  in
  let x = mk_int_var 5 in
  expect "IntExpr parameter accepts equal value" (fun () ->
      check_app f [ Dimensions [ x ]; LiteralInt 4 ] = Int);
  expect "IntExpr parameter rejects other value" (fun () ->
      rejects f [ Dimensions [ x ]; LiteralInt 5 ]);
  expect "IntExpr parameter can't intro vars" (fun () ->
      rejects ~kind:true ([ ("i", IntExpr (Id "m")) ], TypeInt) [ LiteralInt 1 ])
