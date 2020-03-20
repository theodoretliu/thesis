let ctx = Z3.mk_context []
let solver = Z3.Solver.mk_simple_solver ctx


let mk_string =
  let i = ref 0 in
  fun () ->
    let res = "var" ^ string_of_int !i in
    i := !i + 1;
    res


let mk_int = Z3.Arithmetic.Integer.mk_const_s ctx


let prove f =
  let res = Z3.Solver.check solver [Z3.Boolean.mk_not ctx f] in
  res = Z3.Solver.UNSATISFIABLE


let prove_int_eq i1 i2 =
  prove (Z3.Boolean.mk_eq ctx i1 i2)


let add_to_solver (e : Z3.Expr.expr) =
  let new_var_name = mk_string () in
  let new_var = mk_int new_var_name in
  let equality = Z3.Boolean.mk_eq ctx new_var e in
  Z3.Solver.add solver [equality] ;
  new_var_name
