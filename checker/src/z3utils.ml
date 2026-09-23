let ctx = Z3.mk_context []
let solver = Z3.Solver.mk_simple_solver ctx

let mk_string =
  let i = ref 0 in
  fun () ->
    let res = "var" ^ string_of_int !i in
    i := !i + 1;
    res

let mk_int = Z3.Arithmetic.Integer.mk_const_s ctx
let mk_int_numeral = Z3.Arithmetic.Integer.mk_numeral_i ctx

let mk_int_var i =
  let var_name = mk_string () in
  let int_num = mk_int_numeral i in
  let var_expr = mk_int var_name in
  let equality = Z3.Boolean.mk_eq ctx int_num var_expr in
  Z3.Solver.add solver [ equality ];
  var_name

let prove f =
  let res = Z3.Solver.check solver [ Z3.Boolean.mk_not ctx f ] in
  res = Z3.Solver.UNSATISFIABLE

let prove_int_eq i1 i2 = prove (Z3.Boolean.mk_eq ctx i1 i2)
let add_int (xs : string list) = Z3.Arithmetic.mk_add ctx (List.map mk_int xs)
let mul_int (xs : string list) = Z3.Arithmetic.mk_mul ctx (List.map mk_int xs)

(* dimensions are natural numbers *)
let assume_dim (name : string) =
  Z3.Solver.add solver
    [ Z3.Arithmetic.mk_ge ctx (mk_int name) (mk_int_numeral 0) ]

let fresh_dim () =
  let name = mk_string () in
  assume_dim name;
  name

let add_to_solver (e : Z3.Expr.expr) =
  let new_var_name = fresh_dim () in
  Z3.Solver.add solver [ Z3.Boolean.mk_eq ctx (mk_int new_var_name) e ];
  new_var_name
