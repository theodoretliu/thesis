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

(* the unique integer value the solver's assertions force e to take, if any *)
let determined_int (e : Z3.Expr.expr) : int option =
  match Z3.Solver.check solver [] with
  | Z3.Solver.SATISFIABLE -> (
      match Z3.Solver.get_model solver with
      | None -> None
      | Some model -> (
          match Z3.Model.eval model e true with
          | Some value
            when Z3.Arithmetic.is_int_numeral value
                 && prove (Z3.Boolean.mk_eq ctx e value) ->
              int_of_string_opt (Z3.Arithmetic.Integer.numeral_to_string value)
          | _ -> None))
  | _ -> None

(* list variables: a symbolic, unknown-length run of dimensions inside an
   argument's shape, like *B in the body of def f(x: [*B, d]). they share the
   namespace of dimension names; label is the user-facing name *)
let list_vars : (string, string) Hashtbl.t = Hashtbl.create 16
let is_list_var (name : string) = Hashtbl.mem list_vars name
let list_label (name : string) = Hashtbl.find list_vars name

(* the product and number of a list variable's dimensions *)
let prod_of_list (name : string) = mk_int ("prod_" ^ name)
let rank_of_list (name : string) = mk_int ("rank_" ^ name)

let fresh_list ?(label = "") () =
  let name = "list" ^ mk_string () in
  Hashtbl.add list_vars name (if label = "" then name else label);
  let zero = mk_int_numeral 0 in
  Z3.Solver.add solver
    [
      Z3.Arithmetic.mk_ge ctx (prod_of_list name) zero;
      Z3.Arithmetic.mk_ge ctx (rank_of_list name) zero;
    ];
  name
