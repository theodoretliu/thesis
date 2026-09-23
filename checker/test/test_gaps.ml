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
