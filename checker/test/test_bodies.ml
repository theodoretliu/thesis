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

let sg ?(requires = []) ?(exists = []) ?(ensures = []) ?(invariant = [])
    (params, ret) =
  { params; ret; requires; exists; ensures; invariant }

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

(* a module's method, lowered: nn.Linear's forward takes its instance's ints
   first. its invariant is Linear's constructor's requires *)
let lin_params =
  [ ("i", TypeInt); ("o", TypeInt); ("X", arr [ Spread "B"; Id "i" ]) ]

let lin_ret = arr [ Spread "B"; Id "o" ]

let lin =
  sg ~invariant:[ Le (Int 0, Id "i"); Le (Int 0, Id "o") ] (lin_params, lin_ret)

let lin_bare = sg (lin_params, lin_ret)

(* masked_fill(x: [*A], mask: [*#A]) -> [*A]: the mask broadcasts to x *)
let masked_fill =
  sg
    ( [ ("X", arr [ Spread "A" ]); ("M", arr [ Broadcast "A" ]) ],
      arr [ Spread "A" ] )

(* scores(q: [*B, n, d], k: [*B, m, d], mask: [*#B, #n, m]) -> [*B, n, m],
   with attention's mask *)
let scores =
  sg
    ( [
        ("Q", arr [ Spread "B"; Id "n"; Id "d" ]);
        ("K", arr [ Spread "B"; Id "m"; Id "d" ]);
        ("M", arr [ Broadcast "B"; BroadcastDim "n"; Id "m" ]);
      ],
      arr [ Spread "B"; Id "n"; Id "m" ] )

(* transpose(x: [*A], i: int, j: int) -> [*swap(A, i, j)] *)
let transpose =
  sg
    ( [ ("X", arr [ Spread "A" ]); ("i", TypeInt); ("j", TypeInt) ],
      arr [ Swap ("A", Left "i", Left "j") ] )

(* ints: a * b and a // b *)
let mul_int =
  sg ([ ("a", TypeInt); ("b", TypeInt) ], IntExpr (Mul (Id "a", Id "b")))

let div_int =
  sg ([ ("a", TypeInt); ("b", TypeInt) ], IntExpr (Div (Id "a", Id "b")))

(* a constructor whose __init__ asserts n % h == 0 *)
let divides =
  sg
    ~ensures:[ Eq (Mul (Id "h", Div (Id "n", Id "h")), Id "n") ]
    ([ ("n", TypeInt); ("h", TypeInt) ], TypeTuple [])

let env =
  [
    ("linear", Sig linear);
    ("lin", Sig lin);
    ("lin_bare", Sig lin_bare);
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
    ("masked_fill", Sig masked_fill);
    ("scores", Sig scores);
    ("transpose", Sig transpose);
    ("mul", Sig mul_int);
    ("div", Sig div_int);
    ("divides", Sig divides);
  ]

let call f args = Call (f, args)
let var x = Var x

let def ?requires ?exists ?ensures ?invariant name params ret body =
  { name; sg = sg ?requires ?exists ?ensures ?invariant (params, ret); body }

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
  (* jaxtyping's *#A: broadcasting to A, not just with it, so no more dims *)
  expect "[1, 1] doesn't broadcast to a [*B] that may have fewer dims"
    (fun () ->
      rejects
        ~saying:[ "[1, 1] doesn't broadcast to *A = [*B]" ]
        (def "f"
           [ ("x", arr [ Spread "B" ]); ("o", arr [ Int 1; Int 1 ]) ]
           (arr [ Spread "B" ])
           [ Return (call "expand_to" [ var "x"; var "o" ]) ]));
  expect "[1, 1] broadcasts to a [*B] with at least 2 dims" (fun () ->
      checks
        (def
           ~requires:[ Le (Int 2, Rank "B") ]
           "f"
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

(* ---- invariants: an instance's ints satisfy its constructor's requires ---- *)

let () =
  let project f =
    def "f"
      [ ("a", TypeInt); ("b", TypeInt); ("x", arr [ Id "n"; Id "a" ]) ]
      (arr [ Id "n"; Id "b" ])
      [ Return (call f [ var "a"; var "b"; var "x" ]) ]
  in
  expect "a call assumes the callee's invariant" (fun () ->
      checks (project "lin"));
  expect "without one, the callee's returned dim may be negative" (fun () ->
      rejects ~saying:[ "b may be negative" ] (project "lin_bare"));
  let zeros_k ?invariant () =
    def ?invariant "f"
      [ ("k", TypeInt) ]
      (arr [ Id "k" ])
      [ Return (call "zeros" [ var "k" ]) ]
  in
  expect "a body assumes its own invariant" (fun () ->
      checks (zeros_k ~invariant:[ Le (Int 0, Id "k") ] ()));
  expect "without it, the body fails" (fun () ->
      rejects ~saying:[ "may be negative" ] (zeros_k ()));
  expect "invariants don't leak out of the body" (fun () ->
      let before = Z3.Solver.get_num_assertions solver in
      ignore (checks (project "lin"));
      Z3.Solver.get_num_assertions solver = before);
  expect "an invariant the solver can't state is dropped" (fun () ->
      (* h may not be positive, so n // h has no solver term *)
      checks
        (def
           ~invariant:[ Eq (Id "q", Div (Id "n", Id "h")) ]
           "f"
           [
             ("n", TypeInt);
             ("h", TypeInt);
             ("q", TypeInt);
             ("x", arr [ Id "q" ]);
           ]
           (arr [ Id "q" ])
           [ Return (var "x") ]));
  expect "an invariant may only mention the parameters" (fun () ->
      match
        check_signature (sg ~invariant:[ Le (Int 0, Id "z") ] ([], TypeInt))
      with
      | () -> false
      | exception KindError _ -> true)

(* ---- milestone 3 of the Transformer: see docs/15-attention.md ---- *)

let sorted_inferred fd = List.sort compare (inferred fd)
let at_least k x = Le (Int k, Id x)

(* broadcasting: *#A broadcasts to A, #n is n or 1 *)
let () =
  let qk =
    [
      ("q", arr [ Id "b"; Id "n"; Id "d" ]);
      ("k", arr [ Id "b"; Id "m"; Id "d" ]);
    ]
  in
  let masked mask =
    def "f"
      (qk @ [ ("mask", arr mask) ])
      (arr [ Id "b"; Id "n"; Id "m" ])
      [ Return (call "scores" [ var "q"; var "k"; var "mask" ]) ]
  in
  expect "#n accepts n" (fun () -> checks (masked [ Id "b"; Id "n"; Id "m" ]));
  expect "#n accepts 1" (fun () -> checks (masked [ Id "b"; Int 1; Id "m" ]));
  expect "#n rejects another size" (fun () ->
      rejects
        ~saying:[ "expected n or 1, got 2" ]
        (masked [ Id "b"; Int 2; Id "m" ]));
  expect "*#B accepts fewer dims" (fun () -> checks (masked [ Int 1; Id "m" ]));
  expect "*#B accepts 1s for its dims" (fun () ->
      checks (masked [ Int 1; Int 1; Id "m" ]));
  expect "*#B rejects more dims than B" (fun () ->
      rejects
        ~saying:[ "doesn't broadcast to *B" ]
        (masked [ Int 1; Id "b"; Id "n"; Id "m" ]));
  expect "*#B rejects a dim that's neither B's nor 1" (fun () ->
      rejects
        ~saying:[ "doesn't broadcast to *B" ]
        (masked [ Int 2; Id "n"; Id "m" ]));
  (* in a body, *#B and #n are rigid: whatever broadcasts to B, and n or 1 *)
  let attention_params =
    [
      ("q", arr [ Spread "B"; Id "n"; Id "d" ]);
      ("k", arr [ Spread "B"; Id "m"; Id "d" ]);
      ("mask", arr [ Broadcast "B"; BroadcastDim "n"; Id "m" ]);
    ]
  in
  expect "a body's *#B #n mask can be passed on" (fun () ->
      checks
        (def "f" attention_params
           (arr [ Spread "B"; Id "n"; Id "m" ])
           [ Return (call "scores" [ var "q"; var "k"; var "mask" ]) ]));
  expect "a body's *#B #n m mask broadcasts to [*B, n, m]" (fun () ->
      checks
        (def "f"
           (attention_params @ [ ("s", arr [ Spread "B"; Id "n"; Id "m" ]) ])
           (arr [ Spread "B"; Id "n"; Id "m" ])
           [ Return (call "masked_fill" [ var "s"; var "mask" ]) ]));
  expect "and to [*C, *B, n, m]" (fun () ->
      checks
        (def "f"
           (attention_params
           @ [ ("s", arr [ Spread "C"; Spread "B"; Id "n"; Id "m" ]) ])
           (arr [ Spread "C"; Spread "B"; Id "n"; Id "m" ])
           [ Return (call "masked_fill" [ var "s"; var "mask" ]) ]));
  expect "but not to another spread" (fun () ->
      rejects
        ~saying:[ "doesn't broadcast to *A" ]
        (def "f"
           (attention_params @ [ ("s", arr [ Spread "C"; Id "n"; Id "m" ]) ])
           (arr [ Spread "C"; Id "n"; Id "m" ])
           [ Return (call "masked_fill" [ var "s"; var "mask" ]) ]));
  expect "#n isn't n" (fun () ->
      rejects ~saying:[ "got [*#B, #n, m]" ]
        (def "f" attention_params
           (arr [ Spread "B"; Id "n"; Id "m" ])
           [ Return (var "mask") ]));
  expect "#n binds an unbound n" (fun () ->
      checks
        (def "f"
           [ ("x", arr [ BroadcastDim "n" ]); ("y", arr [ Id "n" ]) ]
           (arr [ Id "n" ])
           [ Return (var "y") ]));
  expect "#n can't be in a return type" (fun () ->
      match
        check_signature
          (sg ([ ("x", arr [ Id "n" ]) ], arr [ BroadcastDim "n" ]))
      with
      | () -> false
      | exception KindError _ -> true)

(* transpose(i, j) for any literal axes *)
let () =
  let swapped params i j ret =
    def "f" params (arr ret)
      [ Return (call "transpose" [ var "x"; Lit i; Lit j ]) ]
  in
  expect "swap(A, 1, 2)" (fun () ->
      checks
        (swapped
           [ ("x", arr [ Id "a"; Id "b"; Id "c"; Id "d" ]) ]
           1 2
           [ Id "a"; Id "c"; Id "b"; Id "d" ]));
  expect "swap(A, -2, -1) past a spread" (fun () ->
      checks
        (swapped
           [ ("x", arr [ Spread "L"; Id "a"; Id "b" ]) ]
           (-2) (-1)
           [ Spread "L"; Id "b"; Id "a" ]));
  expect "swap(A, 0, -1) around a spread" (fun () ->
      checks
        (swapped
           [ ("x", arr [ Id "a"; Spread "L"; Id "b" ]) ]
           0 (-1)
           [ Id "b"; Spread "L"; Id "a" ]));
  expect "swap(A, 0, 1) after a spread is ambiguous" (fun () ->
      rejects
        ~saying:[ "Cannot compute Swap(A, i, j) for A = [*L, a, b]" ]
        (swapped
           [ ("x", arr [ Spread "L"; Id "a"; Id "b" ]) ]
           0 1
           [ Spread "L"; Id "b"; Id "a" ]))

(* asserts: the rest of the body assumes them *)
let () =
  (* def f(n: int, h: int, x: [b, n]) -> [b, h, n // h]:
       assert n % h == 0
       return x.reshape(x.size(0), h, n // h) *)
  let split ?requires asserted =
    def ?requires "f"
      [ ("n", TypeInt); ("h", TypeInt); ("x", arr [ Id "b"; Id "n" ]) ]
      (arr [ Id "b"; Id "h"; Div (Id "n", Id "h") ])
      ((if asserted then
          [ Assume (Eq (Mul (Id "h", Div (Id "n", Id "h")), Id "n")) ]
        else [])
      @ [
          Let ("k", call "div" [ var "n"; var "h" ]);
          Return
            (call "reshape"
               [
                 var "x";
                 Shape [ call "size" [ var "x"; Lit 0 ]; var "h"; var "k" ];
               ]);
        ])
  in
  let given = [ at_least 1 "h"; at_least 0 "n" ] in
  expect "an assert is assumed" (fun () -> checks (split ~requires:given true));
  expect "without it, the sizes may not match" (fun () ->
      rejects
        ~saying:[ "Precondition not provable" ]
        (split ~requires:given false));
  (* n >= 0 isn't needed: n is also a dim, of x *)
  expect "an assert's divisor is inferred positive" (fun () ->
      inferred (split true) = [ show_constr (at_least 1 "h") ]);
  expect "an assert about a tensor is dropped" (fun () ->
      checks
        (def "f"
           [ ("x", arr [ Id "n" ]) ]
           (arr [ Id "n" ])
           [ Assume (Le (Id "x", Int 3)); Return (var "x") ]));
  expect "and infers nothing, even for its divisor" (fun () ->
      inferred
        (def "f"
           [ ("k", TypeInt); ("x", arr [ Id "n" ]) ]
           (arr [ Id "n" ])
           [ Assume (Eq (Div (Id "k", Id "k"), Id "x")); Return (var "x") ])
      = []);
  (* def fold(x: [b, n], k: int) -> [b, k, n // k]:
       assert x.size(1) % k == 0
       return x.reshape(x.size(0), k, -1) *)
  let fold asserted =
    def
      ~requires:[ at_least 1 "k"; at_least 1 "b" ]
      "fold"
      [ ("k", TypeInt); ("x", arr [ Id "b"; Id "n" ]) ]
      (arr [ Id "b"; Id "k"; Div (Id "n", Id "k") ])
      ([ Let ("size", call "size" [ var "x"; Lit 1 ]) ]
      @ (if asserted then
           [ Assume (Eq (Mul (Id "k", Div (Id "size", Id "k")), Id "size")) ]
         else [])
      @ [
          Return
            (call "reshape"
               [
                 var "x";
                 Shape [ call "size" [ var "x"; Lit 0 ]; var "k"; Lit (-1) ];
               ]);
        ])
  in
  expect "-1 when an assert says k divides a factor" (fun () ->
      checks (fold true));
  expect "without it, -1 may not divide evenly" (fun () ->
      rejects ~saying:[ "b * n may not be divisible by b * k" ] (fold false));
  expect "a callee's ensures with a divisor it can't state is dropped"
    (fun () ->
      checks
        (def "f"
           [ ("n", TypeInt); ("h", TypeInt) ]
           (TypeTuple [])
           [ Return (call "divides" [ var "n"; var "h" ]) ]))

(* multi-head attention's split and merge: -1 is the quotient when the other
   sizes multiply to some of the total's, h * (d // h) = d *)
let () =
  let invariant =
    [
      at_least 1 "h";
      at_least 0 "d";
      Eq (Mul (Id "h", Div (Id "d", Id "h")), Id "d");
    ]
  in
  let dk = Div (Id "d", Id "h") in
  let heads ?requires () =
    def ?requires ~invariant "heads"
      [ ("h", TypeInt); ("d", TypeInt); ("x", arr [ Id "b"; Id "n"; Id "d" ]) ]
      (arr [ Id "b"; Id "h"; Id "n"; dk ])
      [
        Let ("k", call "div" [ var "d"; var "h" ]);
        Let
          ( "y",
            call "reshape"
              [
                var "x";
                Shape
                  [ call "size" [ var "x"; Lit 0 ]; Lit (-1); var "h"; var "k" ];
              ] );
        Return (call "transpose" [ var "y"; Lit 1; Lit 2 ]);
      ]
  in
  let nonempty = [ at_least 1 "b"; at_least 1 "d" ] in
  expect "split heads with -1" (fun () -> checks (heads ~requires:nonempty ()));
  expect "which needs b >= 1 and d >= 1" (fun () ->
      sorted_inferred (heads ())
      = List.sort compare (List.map show_constr nonempty));
  expect "merge heads with -1, where an array's shape uses the invariant"
    (fun () ->
      checks
        (def ~requires:nonempty ~invariant "merge"
           [
             ("h", TypeInt);
             ("d", TypeInt);
             ("x", arr [ Id "b"; Id "h"; Id "n"; dk ]);
           ]
           (arr [ Id "b"; Id "n"; Id "d" ])
           [
             Let ("y", call "transpose" [ var "x"; Lit 1; Lit 2 ]);
             Let ("hk", call "mul" [ var "h"; call "div" [ var "d"; var "h" ] ]);
             Return
               (call "reshape"
                  [
                    var "y";
                    Shape [ call "size" [ var "y"; Lit 0 ]; Lit (-1); var "hk" ];
                  ]);
           ]))
