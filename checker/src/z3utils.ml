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

(* user-facing names of dimensions, e.g. d for a parameter's dim *)
let dim_labels : (string, string) Hashtbl.t = Hashtbl.create 16

let fresh_dim ?(label = "") () =
  let name = mk_string () in
  assume_dim name;
  if label <> "" then Hashtbl.replace dim_labels name label;
  name

let add_to_solver (e : Z3.Expr.expr) =
  (* a copy of a labeled dimension keeps its label *)
  let label =
    if Z3.Expr.is_const e then
      Option.value ~default:""
        (Hashtbl.find_opt dim_labels (Z3.Expr.to_string e))
    else ""
  in
  let new_var_name = fresh_dim ~label () in
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
      (* the empty list's product is 1 *)
      Z3.Boolean.mk_implies ctx
        (Z3.Boolean.mk_eq ctx (rank_of_list name) zero)
        (Z3.Boolean.mk_eq ctx (prod_of_list name) (mk_int_numeral 1));
    ];
  name

module StringMap = Map.Make (String)

(* unfoldings of list variables the solver proved nonempty: B = [*B', b] or
   B = [b, *B']. each defines fresh variables from B, so recording one assumes
   nothing about B. unfolded variables are replaced everywhere by [expand] *)
let unfoldings : string list StringMap.t ref = ref StringMap.empty

(* how many unfoldings produced a list variable; bounds the search *)
let unfold_depth : (string, int) Hashtbl.t = Hashtbl.create 16
let max_unfold_depth = 8

let can_unfold (name : string) =
  is_list_var name
  && (not (StringMap.mem name !unfoldings))
  && Option.value ~default:0 (Hashtbl.find_opt unfold_depth name)
     < max_unfold_depth
  && prove (Z3.Arithmetic.mk_ge ctx (rank_of_list name) (mk_int_numeral 1))

let unfold ~(right : bool) (name : string) : unit =
  let label = list_label name in
  let rest = fresh_list ~label:(label ^ if right then "[:-1]" else "[1:]") () in
  let dim = fresh_dim ~label:(label ^ if right then "[-1]" else "[0]") () in
  Hashtbl.replace unfold_depth rest
    (1 + Option.value ~default:0 (Hashtbl.find_opt unfold_depth name));
  Z3.Solver.add solver
    [
      Z3.Boolean.mk_eq ctx (rank_of_list name)
        (Z3.Arithmetic.mk_add ctx [ rank_of_list rest; mk_int_numeral 1 ]);
      Z3.Boolean.mk_eq ctx (prod_of_list name)
        (Z3.Arithmetic.mk_mul ctx [ prod_of_list rest; mk_int dim ]);
    ];
  unfoldings :=
    StringMap.add name
      (if right then [ rest; dim ] else [ dim; rest ])
      !unfoldings

(* rewrite unfolded list variables into their parts, and drop list variables
   provably empty *)
let rec expand (l : string list) : string list =
  List.concat_map
    (fun x ->
      if not (is_list_var x) then [ x ]
      else
        match StringMap.find_opt x !unfoldings with
        | Some parts -> expand parts
        | None ->
            if prove (Z3.Boolean.mk_eq ctx (rank_of_list x) (mk_int_numeral 0))
            then []
            else [ x ])
    l

(* run f with its own solver assertions and unfoldings, e.g. a function's
   requires while checking its body *)
let scoped (f : unit -> 'a) : 'a =
  let saved = !unfoldings in
  Z3.Solver.push solver;
  Fun.protect
    ~finally:(fun () ->
      Z3.Solver.pop solver 1;
      unfoldings := saved)
    f
