open Numpy_checking.Typing
open Numpy_checking.Z3utils

(* tests for checking function bodies; see docs/10-function-bodies.md *)

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

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

let sg ?(requires = []) ?(exists = []) ?(ensures = []) (params, ret) =
  { params; ret; requires; exists; ensures }

(* ---- the library bodies call ---- *)

let arr l = Nparray l

(* linear(x: [*A, k], w: [k, p]) -> [*A, p] *)
let linear =
  sg
    ( [ ("X", arr [ Spread "A"; Id "k" ]); ("W", arr [ Id "k"; Id "p" ]) ],
      arr [ Spread "A"; Id "p" ] )

let relu = sg ([ ("X", arr [ Spread "A" ]) ], arr [ Spread "A" ])

(* matmul(x: [*A, m, k], y: [*B, k, p]) -> [Broadcasted(A, B), m, p] *)
let matmul =
  sg
    ( [
        ("X", arr [ Spread "A"; Id "m"; Id "k" ]);
        ("Y", arr [ Spread "B"; Id "k"; Id "p" ]);
      ],
      arr [ Broadcasted [ "A"; "B" ]; Id "m"; Id "p" ] )

let add =
  sg
    ( [ ("X", arr [ Spread "A" ]); ("Y", arr [ Spread "B" ]) ],
      arr [ Broadcasted [ "A"; "B" ] ] )

let sum_axis =
  sg
    ( [ ("X", arr [ Spread "A" ]); ("axis", TypeInt) ],
      arr [ Drop ("A", [ Left "axis" ]) ] )

let shape0 = sg ([ ("X", arr [ Id "n"; Spread "R" ]) ], IntExpr (Id "n"))
let zeros = sg ([ ("k", TypeInt) ], arr [ Id "k" ])

let flatten1 =
  sg ([ ("X", arr [ Id "b"; Spread "A" ]) ], arr [ Id "b"; Prod "A" ])

let unique =
  sg ~exists:[ "m" ]
    ~ensures:[ Le (Id "m", Id "n") ]
    ([ ("X", arr [ Id "n" ]) ], arr [ Id "m" ])

let head =
  sg
    ~requires:[ Le (Id "m", Id "n") ]
    ([ ("X", arr [ Id "n" ]); ("Y", arr [ Id "m" ]) ], arr [ Id "m" ])

(* expands a list of 1s to any shape *)
let expand_to =
  sg
    ( [ ("X", arr [ Spread "A" ]); ("Y", arr [ Broadcast "A" ]) ],
      arr [ Spread "A" ] )

(* zeros_n(size: [*S]) -> [*S], where the frontend passes an int tuple as a
   shape *)
let zeros_n = sg ([ ("size", arr [ Spread "S" ]) ], arr [ Spread "S" ])

let reshape =
  sg
    ~requires:[ Eq (Prod "A", Prod "B") ]
    ( [ ("X", arr [ Spread "A" ]); ("shape", arr [ Spread "B" ]) ],
      arr [ Spread "B" ] )

(* x.size(i) *)
let size =
  sg
    ( [ ("X", arr [ Spread "A" ]); ("i", TypeInt) ],
      IntExpr (Index ("A", Left "i")) )

(* ones(n: int) -> [n], which callers must pass n >= 0, as if inferred *)
let ones =
  sg ~requires:[ Le (Int 0, Id "n") ] ([ ("n", TypeInt) ], arr [ Id "n" ])

(* pair(x: [n], y: [m]) -> ([n], [m]) *)
let pair =
  sg
    ( [ ("X", arr [ Id "n" ]); ("Y", arr [ Id "m" ]) ],
      TypeTuple [ arr [ Id "n" ]; arr [ Id "m" ] ] )

(* conv-like: the kernel must fit; the int overload is tried second *)
let fit =
  Overloads
    [
      sg
        ~requires:[ Le (Id "k", Id "n") ]
        ([ ("X", arr [ Id "n" ]); ("K", arr [ Id "k" ]) ], arr [ Id "n" ]);
      sg ([ ("X", arr [ Id "n" ]); ("i", TypeInt) ], arr [ Id "n" ]);
    ]

let env =
  [
    ("linear", Sig linear);
    ("relu", Sig relu);
    ("matmul", Sig matmul);
    ("add", Sig add);
    ("sum", Sig sum_axis);
    ("shape0", Sig shape0);
    ("zeros", Sig zeros);
    ("flatten", Sig flatten1);
    ("unique", Sig unique);
    ("head", Sig head);
    ("expand_to", Sig expand_to);
    ("zeros_n", Sig zeros_n);
    ("reshape", Sig reshape);
    ("size", Sig size);
    ("ones", Sig ones);
    ("pair", Sig pair);
    ("fit", fit);
  ]

let call f args = Call (f, args)
let var x = Var x

let def ?requires ?exists ?ensures name params ret body =
  { name; sg = sg ?requires ?exists ?ensures (params, ret); body }

let checks fd =
  match check_fundef env fd with
  | () -> true
  | exception (TypeError m | KindError m) ->
      print_endline ("  unexpected error: " ^ m);
      false

let error_of fd =
  match check_fundef env fd with
  | () -> ""
  | exception (TypeError m | KindError m) -> m

let rejects ?(saying = []) fd =
  let m = error_of fd in
  (* SHOW_ERRORS=1 dune exec test/test_bodies.exe prints every diagnostic *)
  if Sys.getenv_opt "SHOW_ERRORS" <> None then print_endline ("  " ^ m);
  (m <> "" && List.for_all (contains m) saying)
  ||
  (print_endline ("  got: " ^ if m = "" then "no error" else m);
   false)

let rank_ge_1 b = Le (Int 1, Rank b)

(* ---- a body is checked once, generically in its variadics ---- *)

(* def mlp(x: [*B, d], w1: [d, h], w2: [h, p]) -> [*B, p] *)
let mlp_params =
  [
    ("x", arr [ Spread "B"; Id "d" ]);
    ("w1", arr [ Id "d"; Id "h" ]);
    ("w2", arr [ Id "h"; Id "p" ]);
  ]

let mlp_body out =
  [
    Let ("y", call "linear" [ var "x"; var "w1" ]);
    Let ("z", call "relu" [ var "y" ]);
    Return (call "linear" [ var "z"; var out ]);
  ]

let mlp = def "mlp" mlp_params (arr [ Spread "B"; Id "p" ]) (mlp_body "w2")

let () =
  expect "mlp checks for every B" (fun () -> checks mlp);
  expect "wrong weight is reported at its call" (fun () ->
      rejects
        ~saying:[ "in mlp"; "linear(z, w1)"; "parameter W" ]
        { mlp with body = mlp_body "w1" });
  expect "wrong declared return is reported" (fun () ->
      rejects
        ~saying:[ "in mlp"; "return value"; "expected p, got h" ]
        { mlp with body = [ Return (call "linear" [ var "x"; var "w1" ]) ] });
  expect "B is rigid: it can't be assumed empty" (fun () ->
      rejects ~saying:[ "return value"; "*B" ]
        (def "f"
           [ ("x", arr [ Spread "B"; Id "d" ]) ]
           (arr [ Id "d" ])
           [ Return (var "x") ]));
  expect "B is rigid: sum over axis 0 depends on it" (fun () ->
      rejects ~saying:[ "sum" ]
        (def "f"
           [ ("x", arr [ Spread "B"; Id "d" ]) ]
           (arr [ Spread "B" ])
           [ Return (call "sum" [ var "x"; Lit 0 ]) ]));
  expect "sum over the last axis gives [*B]" (fun () ->
      checks
        (def "f"
           [ ("x", arr [ Spread "B"; Id "d" ]) ]
           (arr [ Spread "B" ])
           [ Return (call "sum" [ var "x"; Lit (-1) ]) ]));
  expect "a callee's own variadic binds to a list containing B" (fun () ->
      checks
        (def "f"
           [ ("x", arr [ Id "n"; Spread "B" ]) ]
           (arr [ Id "n"; Prod "B" ])
           [ Return (call "flatten" [ var "x" ]) ]))

(* ---- annotations, int parameters, errors ---- *)

let () =
  expect "annotated local checks and binds a name" (fun () ->
      checks
        {
          mlp with
          body =
            [
              LetAnnot
                ( "y",
                  arr [ Spread "B"; Id "hid" ],
                  call "linear" [ var "x"; var "w1" ] );
              LetAnnot
                ("z", arr [ Spread "B"; Id "hid" ], call "relu" [ var "y" ]);
              Return (call "linear" [ var "z"; var "w2" ]);
            ];
        });
  expect "wrong annotation rejected" (fun () ->
      rejects
        ~saying:[ "annotation of y"; "expected p, got h" ]
        {
          mlp with
          body =
            [
              LetAnnot
                ( "y",
                  arr [ Spread "B"; Id "p" ],
                  call "linear" [ var "x"; var "w1" ] );
              Return (var "y");
            ];
        });
  expect "int parameter may be negative" (fun () ->
      rejects ~saying:[ "k may be negative" ]
        (def "f"
           [ ("k", TypeInt) ]
           (arr [ Id "k" ])
           [ Return (call "zeros" [ var "k" ]) ]));
  expect "int parameter flows into shapes given 0 <= k" (fun () ->
      checks
        (def
           ~requires:[ Le (Int 0, Id "k") ]
           "f"
           [ ("k", TypeInt) ]
           (arr [ Id "k" ])
           [ Return (call "zeros" [ var "k" ]) ]));
  expect "x.shape[0] flows into zeros" (fun () ->
      checks
        (def "f"
           [ ("x", arr [ Id "n"; Id "d" ]) ]
           (arr [ Id "n" ])
           [
             Let ("n", call "shape0" [ var "x" ]);
             Return (call "zeros" [ var "n" ]);
           ]));
  expect "unbound variable" (fun () ->
      rejects ~saying:[ "unbound variable y" ]
        (def "f" [] (arr []) [ Return (var "y") ]));
  expect "unknown function" (fun () ->
      rejects ~saying:[ "g: unknown function" ]
        (def "f" [] (arr []) [ Return (call "g" []) ]));
  expect "missing return" (fun () ->
      rejects ~saying:[ "missing return" ] (def "f" [] (arr []) []));
  expect "ill-kinded signature" (fun () ->
      rejects ~saying:[ "in f" ]
        (def "f" [] (arr [ Id "undeclared" ]) [ Return (call "g" []) ]))

(* ---- requires and ensures ---- *)

let head_body = [ Return (call "head" [ var "x"; var "y" ]) ]
let head_params = [ ("x", arr [ Id "n" ]); ("y", arr [ Id "m" ]) ]

let () =
  expect "a body may assume its requires" (fun () ->
      checks
        (def
           ~requires:[ Le (Id "m", Id "n") ]
           "f" head_params (arr [ Id "m" ]) head_body));
  expect "without the requires, the callee's precondition fails" (fun () ->
      rejects
        ~saying:[ "Precondition not provable" ]
        (def "f" head_params (arr [ Id "m" ]) head_body));
  expect "requires don't leak out of the body" (fun () ->
      let before = Z3.Solver.get_num_assertions solver in
      ignore
        (checks
           (def
              ~requires:[ Le (Id "m", Id "n") ]
              "f" head_params (arr [ Id "m" ]) head_body));
      Z3.Solver.get_num_assertions solver = before);
  expect "contradictory requires rejected" (fun () ->
      rejects ~saying:[ "contradictory" ]
        (def
           ~requires:[ Lt (Id "n", Int 0) ]
           "f"
           [ ("x", arr [ Id "n" ]) ]
           (arr [ Id "n" ])
           [ Return (var "x") ]))

let unique_params = [ ("x", arr [ Id "n" ]) ]

let () =
  expect "existential bound by the return, ensures proven" (fun () ->
      checks
        (def ~exists:[ "k" ]
           ~ensures:[ Le (Id "k", Id "n") ]
           "f" unique_params (arr [ Id "k" ])
           [ Return (call "unique" [ var "x" ]) ]));
  expect "returning x itself also satisfies k <= n" (fun () ->
      checks
        (def ~exists:[ "k" ]
           ~ensures:[ Le (Id "k", Id "n") ]
           "f" unique_params (arr [ Id "k" ])
           [ Return (var "x") ]));
  expect "unprovable ensures rejected" (fun () ->
      rejects
        ~saying:[ "Postcondition not provable: k" ]
        (def ~exists:[ "k" ]
           ~ensures:[ Lt (Id "k", Id "n") ]
           "f" unique_params (arr [ Id "k" ])
           [ Return (call "unique" [ var "x" ]) ]));
  expect "existential not in the return type" (fun () ->
      rejects ~saying:[ "doesn't determine k" ]
        (def ~exists:[ "k" ]
           ~ensures:[ Le (Id "k", Id "n") ]
           "f" unique_params (arr [ Id "n" ])
           [ Return (var "x") ]))

(* ---- rank-guided unfolding ---- *)

let bmm_params =
  [ ("x", arr [ Spread "B"; Id "d" ]); ("w", arr [ Id "d"; Id "p" ]) ]

let bmm_body = [ Return (call "matmul" [ var "x"; var "w" ]) ]

let () =
  expect "batched matmul needs rank(B) >= 1" (fun () ->
      rejects ~saying:[ "matmul" ]
        (def "f" bmm_params (arr [ Spread "B"; Id "p" ]) bmm_body));
  expect "batched matmul with rank(B) >= 1 gives [*B, p]" (fun () ->
      checks
        (def
           ~requires:[ rank_ge_1 "B" ]
           "f" bmm_params
           (arr [ Spread "B"; Id "p" ])
           bmm_body));
  expect "B's first dim with rank(B) >= 1" (fun () ->
      checks
        (def
           ~requires:[ rank_ge_1 "B" ]
           "f"
           [ ("x", arr [ Spread "B"; Id "d" ]) ]
           (arr [ Spread "B"; Id "d" ])
           [
             Let ("n", call "shape0" [ var "x" ]);
             Let ("z", call "zeros" [ var "n" ]);
             Return (var "x");
           ]));
  expect "B's first dim without a rank rejected" (fun () ->
      rejects
        ~saying:[ "unknown number of dimensions" ]
        (def "f"
           [ ("x", arr [ Spread "B"; Id "d" ]) ]
           TypeInt
           [ Return (call "shape0" [ var "x" ]) ]));
  expect "rank(B) = 2 lets B match [a, b]" (fun () ->
      checks
        (def
           ~requires:[ Eq (Rank "B", Int 2) ]
           "f"
           [ ("x", arr [ Spread "B"; Id "d" ]) ]
           (arr [ Spread "B"; Id "d" ])
           [
             LetAnnot ("y", arr [ Id "a"; Id "b"; Id "d" ], var "x");
             Return (var "y");
           ]));
  expect "rank(B) >= 1 doesn't make B two dims" (fun () ->
      rejects ~saying:[ "annotation of y" ]
        (def
           ~requires:[ rank_ge_1 "B" ]
           "f"
           [ ("x", arr [ Spread "B"; Id "d" ]) ]
           (arr [ Spread "B"; Id "d" ])
           [
             LetAnnot ("y", arr [ Id "a"; Id "b"; Id "d" ], var "x");
             Return (var "y");
           ]));
  expect "rank(B) = 0 makes B empty" (fun () ->
      checks
        (def
           ~requires:[ Eq (Rank "B", Int 0) ]
           "f"
           [ ("x", arr [ Spread "B"; Id "d" ]) ]
           (arr [ Id "d" ])
           [ Return (var "x") ]));
  expect "the empty list's product is 1" (fun () ->
      checks
        (def
           ~requires:[ Eq (Rank "B", Int 0) ]
           "f"
           [ ("x", arr [ Id "n"; Spread "B" ]) ]
           (arr [ Id "n"; Int 1 ])
           [ Return (call "flatten" [ var "x" ]) ]));
  expect "prod(B) = 1 when B is empty, without unfolding" (fun () ->
      let b = fresh_list ~label:"B" () in
      Z3.Solver.push solver;
      Z3.Solver.add solver
        [ Z3.Boolean.mk_eq ctx (rank_of_list b) (mk_int_numeral 0) ];
      let r = prove_int_eq (prod_of_list b) (mk_int_numeral 1) in
      Z3.Solver.pop solver 1;
      r)

(* ---- 1s broadcast against list variables ---- *)

let () =
  let params =
    [ ("x", arr [ Spread "B"; Id "d" ]); ("y", arr [ Int 1; Id "d" ]) ]
  in
  let body = [ Return (call "add" [ var "x"; var "y" ]) ] in
  expect "x + [1, d] needs B nonempty to have shape [*B, d]" (fun () ->
      rejects ~saying:[ "add" ]
        (def "f" params (arr [ Spread "B"; Id "d" ]) body));
  expect "x + [1, d] with rank(B) >= 1 is [*B, d]" (fun () ->
      checks
        (def
           ~requires:[ rank_ge_1 "B" ]
           "f" params
           (arr [ Spread "B"; Id "d" ])
           body));
  expect "[1, 1] broadcasts to any [*B]" (fun () ->
      checks
        (def "f"
           [ ("x", arr [ Spread "B" ]); ("o", arr [ Int 1; Int 1 ]) ]
           (arr [ Spread "B" ])
           [ Return (call "expand_to" [ var "x"; var "o" ]) ]));
  expect "[2] does not broadcast to any [*B]" (fun () ->
      rejects ~saying:[ "expand_to" ]
        (def "f"
           [ ("x", arr [ Spread "B" ]); ("o", arr [ Int 2 ]) ]
           (arr [ Spread "B" ])
           [ Return (call "expand_to" [ var "x"; var "o" ]) ]))

(* ---- programs: callers see only signatures ---- *)

let () =
  let block =
    def "block"
      [ ("x", arr [ Spread "B"; Id "d" ]); ("w", arr [ Id "d"; Id "d" ]) ]
      (arr [ Spread "B"; Id "d" ])
      [
        Let ("h", call "linear" [ var "x"; var "w" ]);
        Return (call "add" [ var "x"; call "relu" [ var "h" ] ]);
      ]
  in
  let net ret =
    def "net"
      [
        ("x", arr [ Id "n"; Id "t"; Id "d" ]);
        ("w", arr [ Id "d"; Id "d" ]);
        ("out", arr [ Id "d"; Int 10 ]);
      ]
      ret
      [
        Let ("h", call "block" [ var "x"; var "w" ]);
        Let ("h", call "block" [ var "h"; var "w" ]);
        Return (call "linear" [ var "h"; var "out" ]);
      ]
  in
  expect "a checked function is called through its signature" (fun () ->
      match
        check_program env [ block; net (arr [ Id "n"; Id "t"; Int 10 ]) ]
      with
      | () -> true
      | exception (TypeError m | KindError m) ->
          print_endline ("  unexpected error: " ^ m);
          false);
  expect "a caller's mistake is reported in the caller" (fun () ->
      match check_program env [ block; net (arr [ Id "n"; Int 10 ]) ] with
      | () -> false
      | exception TypeError m -> contains m "in net"
      | exception KindError _ -> false);
  expect "recursive call through the signature" (fun () ->
      checks
        (def "loop"
           [ ("x", arr [ Spread "B" ]) ]
           (arr [ Spread "B" ])
           [ Return (call "loop" [ var "x" ]) ]))

(* ---- what the frontend emits: shapes from ints, floats, locations ---- *)

let () =
  let n_param = [ ("n", TypeLiteralInt 3) ] in
  expect "a tuple of ints is a shape" (fun () ->
      checks
        (def "f" n_param
           (arr [ Int 3; Int 2 ])
           [ Return (call "zeros_n" [ Shape [ var "n"; Lit 2 ] ]) ]));
  expect "a shape of the wrong size is caught" (fun () ->
      rejects ~saying:[ "expected 2, got 3" ]
        (def "f" n_param (arr [ Int 2 ])
           [ Return (call "zeros_n" [ Shape [ var "n" ] ]) ]));
  expect "-1 in a shape needs an equation to determine it" (fun () ->
      rejects
        ~saying:[ "the size -1 can't be inferred here" ]
        (def "f" [] (arr [ Int 2 ])
           [ Return (call "zeros_n" [ Shape [ Lit (-1) ] ]) ]));
  expect "other negative sizes are rejected" (fun () ->
      rejects
        ~saying:[ "shape entry -2 is negative" ]
        (def "f" [] (arr [ Int 2 ])
           [ Return (call "zeros_n" [ Shape [ Lit (-2) ] ]) ]));
  expect "an int parameter may be negative" (fun () ->
      rejects
        ~saying:[ "shape entry k may be negative" ]
        (def "f"
           [ ("k", TypeInt) ]
           (arr [ Id "m" ]) ~exists:[ "m" ]
           [ Return (call "zeros_n" [ Shape [ var "k" ] ]) ]));
  expect "an array isn't a shape entry" (fun () ->
      rejects
        ~saying:[ "expected an int in a shape" ]
        (def "f"
           [ ("x", arr [ Id "n" ]) ]
           (arr [ Id "n" ])
           [ Return (call "zeros_n" [ Shape [ var "x" ] ]) ]));
  expect "a float broadcasts like a 0-d array" (fun () ->
      checks
        (def "f"
           [ ("x", arr [ Spread "B"; Id "d" ]) ]
           (arr [ Spread "B"; Id "d" ])
           [ Return (call "add" [ var "x"; Scalar ]) ]));
  expect "errors name the source line and text" (fun () ->
      rejects
        ~saying:[ "in f, line 7, `y = x @ x`: matmul:" ]
        (def "f"
           [ ("x", arr [ Id "n" ]) ]
           (arr [ Id "n" ])
           [
             At (7, "y = x @ x", Let ("y", call "matmul" [ var "x"; var "x" ]));
             At (8, "return y", Return (var "y"));
           ]));
  expect "overloads may have requires" (fun () ->
      checks
        (def "f"
           [ ("x", arr [ Int 5 ]); ("k", arr [ Int 3 ]) ]
           (arr [ Int 5 ])
           [ Return (call "fit" [ var "x"; var "k" ]) ]));
  expect "an overload's requires is checked" (fun () ->
      rejects
        ~saying:[ "Precondition not provable: k = 6 <= n = 5" ]
        (def "f"
           [ ("x", arr [ Int 5 ]); ("k", arr [ Int 6 ]) ]
           (arr [ Int 5 ])
           [ Return (call "fit" [ var "x"; var "k" ]) ]))

(* ---- milestone 1 of the Transformer: see docs/13-free-functions.md ---- *)

let inferred fd =
  match infer_requires env fd with
  | cs -> List.map show_constr cs
  | exception (TypeError m | KindError m) -> [ "error: " ^ m ]

let () =
  expect "x.size(i) is the dim at i" (fun () ->
      checks
        (def "f"
           [ ("x", arr [ Id "b"; Id "t" ]) ]
           (IntExpr (Id "t"))
           [ Return (call "size" [ var "x"; Lit (-1) ]) ]));
  expect "x.size(i) out of range" (fun () ->
      rejects
        ~saying:[ "A[i] is undefined for [b, t]" ]
        (def "f"
           [ ("x", arr [ Id "b"; Id "t" ]) ]
           (IntExpr (Id "t"))
           [ Return (call "size" [ var "x"; Lit 2 ]) ]));
  (* split heads: x.view(x.size(0), -1, 3, 4) *)
  let heads ?requires () =
    def ?requires "f"
      [ ("x", arr [ Id "b"; Id "n"; Int 12 ]) ]
      (arr [ Id "b"; Id "n"; Int 3; Int 4 ])
      [
        Return
          (call "reshape"
             [
               var "x";
               Shape [ call "size" [ var "x"; Lit 0 ]; Lit (-1); Lit 3; Lit 4 ];
             ]);
      ]
  in
  expect "-1 needs the other sizes to be nonzero" (fun () ->
      rejects
        ~saying:[ "the other sizes' product b * 3 * 4 may be 0" ]
        (heads ()));
  expect "-1 is the quotient, given the other sizes are nonzero" (fun () ->
      checks (heads ~requires:[ Le (Int 1, Id "b") ] ()));
  expect "which the body infers" (fun () ->
      inferred (heads ())
      = [ "(Typing.Le ((Typing.Int 1), (Typing.Id \"b\")))" ]);
  expect "-1 must divide evenly" (fun () ->
      rejects
        ~saying:[ "b * t may not be divisible by 3" ]
        (def "f"
           [ ("x", arr [ Id "b"; Id "t" ]) ]
           (arr [ Id "b"; Id "t" ])
           [
             Let ("y", call "reshape" [ var "x"; Shape [ Lit (-1); Lit 3 ] ]);
             Return (var "x");
           ]));
  expect "only one -1" (fun () ->
      rejects
        ~saying:[ "only one size in a shape can be -1" ]
        (def "f"
           [ ("x", arr [ Id "b"; Id "t" ]) ]
           (arr [ Id "b"; Id "t" ])
           [ Return (call "reshape" [ var "x"; Shape [ Lit (-1); Lit (-1) ] ]) ]));
  (* ints as sizes *)
  let zeros_of ?requires () =
    def ?requires "f"
      [ ("n", TypeInt) ]
      (arr [ Id "n" ])
      [ Return (call "zeros_n" [ Shape [ var "n" ] ]) ]
  in
  expect "an int size may be negative" (fun () ->
      rejects ~saying:[ "shape entry n may be negative" ] (zeros_of ()));
  expect "so the body infers n >= 0" (fun () ->
      inferred (zeros_of ())
      = [ "(Typing.Le ((Typing.Int 0), (Typing.Id \"n\")))" ]);
  expect "and checks given it" (fun () ->
      checks (zeros_of ~requires:[ Le (Int 0, Id "n") ] ()));
  expect "an inferred requires is inferred in turn" (fun () ->
      inferred
        (def "g"
           [ ("k", TypeInt) ]
           (arr [ Id "k" ])
           [ Return (call "ones" [ var "k" ]) ])
      = [ "(Typing.Le ((Typing.Int 0), (Typing.Id \"k\")))" ]);
  expect "a relation isn't inferred" (fun () ->
      match
        infer_requires env
          (def "f"
             [ ("x", arr [ Id "n" ]); ("k", arr [ Id "m" ]) ]
             (arr [ Id "n" ])
             [ Return (call "fit" [ var "x"; var "k" ]) ])
      with
      | _ -> false
      | exception TypeError m -> contains m "Precondition not provable");
  expect "an int parameter may come after the shapes naming it" (fun () ->
      checks
        (def "f"
           [ ("x", arr [ Id "n" ]); ("n", TypeInt) ]
           (arr [ Id "n" ])
           [ Return (var "x") ]));
  (* tuples *)
  expect "a tuple return" (fun () ->
      checks
        (def "f"
           [ ("x", arr [ Id "a" ]); ("y", arr [ Id "b" ]) ]
           (TypeTuple [ arr [ Id "a" ]; arr [ Id "b" ] ])
           [ Return (Tup [ var "x"; var "y" ]) ]));
  expect "a tuple's elements are checked" (fun () ->
      rejects ~saying:[ "return value[0]" ]
        (def "f"
           [ ("x", arr [ Id "a" ]); ("y", arr [ Id "b" ]) ]
           (TypeTuple [ arr [ Id "a" ]; arr [ Id "b" ] ])
           [ Return (Tup [ var "y"; var "x" ]) ]));
  expect "unpacking a call's tuple" (fun () ->
      checks
        (def "f"
           [ ("x", arr [ Id "a" ]); ("y", arr [ Id "b" ]) ]
           (arr [ Id "b" ])
           [
             Unpack ([ "p"; "q" ], call "pair" [ var "x"; var "y" ]);
             Return (var "q");
           ]));
  expect "unpacking into the wrong number of names" (fun () ->
      rejects ~saying:[ "into 3 names" ]
        (def "f"
           [ ("x", arr [ Id "a" ]); ("y", arr [ Id "b" ]) ]
           (arr [ Id "b" ])
           [
             Unpack ([ "p"; "q"; "r" ], call "pair" [ var "x"; var "y" ]);
             Return (var "q");
           ]));
  expect "tuple parameters are rejected" (fun () ->
      match
        check_signature (sg ([ ("t", TypeTuple [ TypeInt ]) ], TypeInt))
      with
      | () -> false
      | exception KindError _ -> true)
