open Z3utils

type ('a, 'b) either = Left of 'a | Right of 'b [@@deriving show]

type entry =
  | Id of string
  | Add of entry * entry
  | Sub of entry * entry
  | Mul of entry * entry
  | Div of entry * entry (* floor division; divisor must be provably positive *)
  | Spread of string
  | Drop of string * (string, int) either list
  | Keep of string * (string, int) either list
  | Int of int
  | Broadcast of string
  | Broadcasted of string list
    (* the broadcast of already-bound spreads, e.g. the result of x + y *)
  | Permute of string * (string, int) either list (* result[i] = A[p[i]] *)
  | SetAt of string * (string, int) either list * entry
    (* replace dims at indices, e.g. keepdim=True *)
  | InsertAt of string * (string, int) either * entry
    (* insert a dim, e.g. unsqueeze *)
  | Prod of string (* product of a bound spread's dims, e.g. flatten *)
  | Rank of string (* number of dims in a bound spread *)
  | Index of string * (string, int) either
    (* one dim of a bound spread, e.g. x.size(i) *)
[@@deriving show]

type typ =
  | Nparray of entry list
  | TypeInt
  | IntExpr of entry (* an int whose value is the given dimension expression *)
  | TypeLiteralInt of int (* python Literal[i]; bools are 0 and 1 *)
  | TypeTuple of typ list (* only as a return type *)
[@@deriving show]

type funtyp = (string * typ) list * typ

(* relations between arithmetic dimensions *)
type constr = Eq of entry * entry | Le of entry * entry | Lt of entry * entry
[@@deriving show]

(* a function type with refinements:
   - requires: must be provable from the arguments (preconditions)
   - exists: fresh dimensions the result may use (data-dependent shapes)
   - ensures: assumed about the exists dims afterwards (postconditions) *)
type signature = {
  params : (string * typ) list;
  ret : typ;
  requires : constr list;
  exists : string list;
  ensures : constr list;
}

type arg =
  | Dimensions of string list
  | LiteralInt of int
  | Int (* an int with no known value or identity *)
  | SymInt of string (* an int whose value is the named z3 variable *)
  | Tuple of arg list
[@@deriving show]

exception TypeError of string
exception KindError of string

(* readable forms of shapes for diagnostics *)
let rec string_of_entry (e : entry) : string =
  let idx = function Left s -> s | Right i -> string_of_int i in
  let idxs l = String.concat ", " (List.map idx l) in
  let arith = function
    | (Add _ | Sub _ | Mul _ | Div _) as e -> "(" ^ string_of_entry e ^ ")"
    | e -> string_of_entry e
  in
  match e with
  | Id x -> x
  | Int i -> string_of_int i
  | Add (a, b) -> arith a ^ " + " ^ arith b
  | Sub (a, b) -> arith a ^ " - " ^ arith b
  | Mul (a, b) -> arith a ^ " * " ^ arith b
  | Div (a, b) -> arith a ^ " // " ^ arith b
  | Spread a -> "*" ^ a
  | Drop (a, l) -> "Drop(" ^ a ^ ", [" ^ idxs l ^ "])"
  | Keep (a, l) -> "Keep(" ^ a ^ ", [" ^ idxs l ^ "])"
  | Broadcast a -> "Broadcast(" ^ a ^ ")"
  | Broadcasted l -> "Broadcasted(" ^ String.concat ", " l ^ ")"
  | Permute (a, l) -> "Permute(" ^ a ^ ", [" ^ idxs l ^ "])"
  | SetAt (a, l, d) ->
      "SetAt(" ^ a ^ ", [" ^ idxs l ^ "], " ^ string_of_entry d ^ ")"
  | InsertAt (a, i, d) ->
      "InsertAt(" ^ a ^ ", " ^ idx i ^ ", " ^ string_of_entry d ^ ")"
  | Prod a -> "prod(" ^ a ^ ")"
  | Rank a -> "rank(" ^ a ^ ")"
  | Index (a, i) -> a ^ "[" ^ idx i ^ "]"

(* a solver term as infix arithmetic, with determined parts as numbers and
   dimensions by their labels *)
let rec string_of_expr (e : Z3.Expr.expr) : string =
  match Z3utils.determined_int e with
  | Some i -> string_of_int i
  | None -> (
      let args = Z3.Expr.get_args e in
      let operand a =
        let s = string_of_expr a in
        if Z3.Expr.get_num_args a = 0 || Z3utils.determined_int a <> None then s
        else "(" ^ s ^ ")"
      in
      let infix op = String.concat (" " ^ op ^ " ") (List.map operand args) in
      match Z3.FuncDecl.get_decl_kind (Z3.Expr.get_func_decl e) with
      | _ when Z3.Expr.is_const e -> (
          match Hashtbl.find_opt Z3utils.dim_labels (Z3.Expr.to_string e) with
          | Some label -> label
          | None -> Z3.Expr.to_string e)
      | Z3enums.OP_ADD -> infix "+"
      | Z3enums.OP_SUB -> infix "-"
      | Z3enums.OP_MUL -> infix "*"
      | Z3enums.OP_IDIV -> infix "//"
      | Z3enums.OP_UMINUS -> "-" ^ operand (List.hd args)
      | _ -> Z3.Expr.to_string e)

let string_of_dim (v : string) : string =
  if Z3utils.is_list_var v then "*" ^ Z3utils.list_label v
  else string_of_expr (Z3utils.mk_int v)

(* "name = value", or just the name when the value is only its label *)
let with_value (name : string) (e : Z3.Expr.expr) : string =
  let v = string_of_expr e in
  if v = name then name else name ^ " = " ^ v

let string_of_dims l =
  "[" ^ String.concat ", " (List.map string_of_dim (Z3utils.expand l)) ^ "]"

let rec string_of_typ = function
  | Nparray l ->
      "array[" ^ String.concat ", " (List.map string_of_entry l) ^ "]"
  | TypeInt -> "int"
  | TypeLiteralInt i -> "Literal[" ^ string_of_int i ^ "]"
  | IntExpr e -> "int{" ^ string_of_entry e ^ "}"
  | TypeTuple ts ->
      "tuple[" ^ String.concat ", " (List.map string_of_typ ts) ^ "]"

let rec string_of_arg = function
  | Dimensions l -> "array of shape " ^ string_of_dims l
  | LiteralInt i -> "Literal[" ^ string_of_int i ^ "]"
  | Int -> "int"
  | SymInt v -> "int " ^ string_of_dim v
  | Tuple l -> "tuple (" ^ String.concat ", " (List.map string_of_arg l) ^ ")"

module StringMap = Map.Make (struct
  type t = string

  let compare = compare
end)

module StringSet = Set.Make (struct
  type t = string

  let compare = compare
end)

(* single dimension variables, spread variables, named parameters *)
type mapping_type =
  Z3.Expr.expr StringMap.t * string list StringMap.t * arg StringMap.t

let is_int_param (x : string) (param_var_mapping : typ StringMap.t) : bool =
  match StringMap.find_opt x param_var_mapping with
  | Some TypeInt
  | Some (Nparray [])
  | Some (IntExpr _)
  | Some (TypeLiteralInt _) ->
      true
  | _ -> false

(* arithmetic dimensions may only mention already-bound dimension variables,
   integer parameters, and integer literals *)
let rec check_arith_signature
    ((vars, _spread_vars, param_var_mapping) :
      StringSet.t * StringSet.t * typ StringMap.t) (e : entry) : unit =
  match e with
  | Id x ->
      if not (StringSet.mem x vars || is_int_param x param_var_mapping) then
        raise (KindError ("Unbound variable " ^ x ^ " in arithmetic dimension"))
  | Int _ -> ()
  | Prod a | Rank a ->
      if not (StringSet.mem a _spread_vars) then
        raise
          (KindError (string_of_entry e ^ " needs an already-bound spread var"))
  | Index (a, i) ->
      if not (StringSet.mem a _spread_vars) then
        raise
          (KindError (string_of_entry e ^ " needs an already-bound spread var"));
      begin match i with
      | Left s when not (is_int_param s param_var_mapping) ->
          raise (KindError ("Index to " ^ string_of_entry e ^ " is not an int"))
      | _ -> ()
      end
  | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Div (e1, e2) ->
      check_arith_signature (vars, _spread_vars, param_var_mapping) e1;
      check_arith_signature (vars, _spread_vars, param_var_mapping) e2
  | Spread _ | Drop _ | Keep _ | Broadcast _ | Broadcasted _ | Permute _
  | SetAt _ | InsertAt _ ->
      raise
        (KindError ("List dimension inside arithmetic: " ^ string_of_entry e))

(* given a set of already declared variables and spread variables, this
function checks that the entry type is well-kinded. particularly, it ensures
a few things:
   - new variables are not introduced under arithmetic
   - introduction of new variables do not shadow spread variables and vice versa
   - Spread, Drop, Keep and Broadcast do not occur under arithmetic
   - Id may reference an integer parameter (thesis rule CheckDimIdFoundInParams)
   - Drop needs a spread variable as its first argument and only vars mapping to TypeInts as its second *)
let rec check_entry_signature
    ((vars, spread_vars, param_var_mapping) as orig :
      StringSet.t * StringSet.t * typ StringMap.t) (e : entry)
    (can_intro : bool) : StringSet.t * StringSet.t * typ StringMap.t =
  match e with
  | Id x when is_int_param x param_var_mapping -> orig
  | Id x ->
      if can_intro then
        if StringSet.mem x spread_vars || StringMap.mem x param_var_mapping then
          raise
            (KindError
               "Attempt to intro variable which is already spread or parameter \
                variable")
        else (StringSet.add x vars, spread_vars, param_var_mapping)
      else if StringSet.mem x vars then (vars, spread_vars, param_var_mapping)
      else raise (KindError "Attempt to intro new variable in bad context")
  | Add _ | Sub _ | Mul _ | Div _ | Prod _ | Rank _ | Index _ ->
      check_arith_signature orig e;
      orig
  | Spread x ->
      if can_intro then
        if StringSet.mem x vars || StringMap.mem x param_var_mapping then
          raise
            (KindError
               "Attempt to intro spread variable which is already variable")
        else (vars, StringSet.add x spread_vars, param_var_mapping)
      else if StringSet.mem x spread_vars then
        (vars, spread_vars, param_var_mapping)
      else raise (KindError "Attempt to intro new variable in bad context")
  | Drop _ | Keep _ | Permute _ | SetAt _ | InsertAt _ ->
      (* list functions of a bound spread, indexed by literals or int params *)
      let arr, indices, inserted =
        match e with
        | Drop (arr, indices) | Keep (arr, indices) | Permute (arr, indices) ->
            (arr, indices, None)
        | SetAt (arr, indices, d) -> (arr, indices, Some d)
        | InsertAt (arr, index, d) -> (arr, [ index ], Some d)
        | _ -> failwith "impossible"
      in
      if not (StringSet.mem arr spread_vars) then
        raise
          (KindError
             ("First argument to " ^ string_of_entry e ^ " is not a spread var"))
      else if
        List.exists
          (fun index ->
            match index with
            | Left s -> not (is_int_param s param_var_mapping)
            | Right _ -> false)
          indices
      then
        raise
          (KindError ("Indices to " ^ string_of_entry e ^ " are not integers"))
      else (
        Option.iter (check_arith_signature orig) inserted;
        orig)
  | Int i ->
      if i < 0 then raise (KindError "Negative dimension literal") else orig
  | Broadcast s ->
      if not (StringSet.mem s spread_vars) then
        raise (KindError "Argument to broadcast is not already a spread var")
      else orig
  | Broadcasted names ->
      if
        names = []
        || not (List.for_all (fun s -> StringSet.mem s spread_vars) names)
      then raise (KindError "Broadcasted needs already-bound spread vars")
      else orig

(* int parameters come first, so a shape may name one declared after it *)
let ints_first (params : (string * typ) list) : (string * typ) list =
  let is_int (_, t) =
    match t with TypeInt | TypeLiteralInt _ -> true | _ -> false
  in
  let ints, rest = List.partition is_int params in
  ints @ rest

let check_args_signature (funargtyps : (string * typ) list) :
    StringSet.t * StringSet.t * typ StringMap.t =
  let check_args_signature'
      ((vars, spread_vars, param_var_mapping) :
        StringSet.t * StringSet.t * typ StringMap.t)
      ((param_name, arg) : string * typ) :
      StringSet.t * StringSet.t * typ StringMap.t =
    (* don't allow parameter names to be in the set of already used variables *)
    if
      List.exists (StringSet.mem param_name) [ vars; spread_vars ]
      || StringMap.mem param_name param_var_mapping
    then raise (KindError "Parameter name already used")
    else
      let new_param_var_mapping =
        StringMap.add param_name arg param_var_mapping
      in

      match arg with
      | TypeInt | TypeLiteralInt _ -> (vars, spread_vars, new_param_var_mapping)
      | TypeTuple _ -> raise (KindError "Tuple parameters aren't supported")
      | IntExpr e ->
          (* e may only mention what earlier parameters bound *)
          check_arith_signature (vars, spread_vars, param_var_mapping) e;
          (vars, spread_vars, new_param_var_mapping)
      | Nparray l ->
          let new_vars, new_spread_vars, new_param_var_mapping =
            List.fold_left
              (fun acc e -> check_entry_signature acc e true)
              (vars, spread_vars, new_param_var_mapping)
              l
          in
          (new_vars, new_spread_vars, new_param_var_mapping)
  in

  List.fold_left check_args_signature'
    (StringSet.empty, StringSet.empty, StringMap.empty)
    (ints_first funargtyps)

let check_constr_signature ctx (c : constr) : unit =
  match c with
  | Eq (e1, e2) | Le (e1, e2) | Lt (e1, e2) ->
      check_arith_signature ctx e1;
      check_arith_signature ctx e2

let check_signature (sg : signature) : unit =
  let ((vars, spread_vars, param_vars) as ctx) =
    check_args_signature sg.params
  in
  List.iter (check_constr_signature ctx) sg.requires;

  (* exists dims behave like dimension variables bound by the parameters *)
  let vars =
    List.fold_left
      (fun vars x ->
        if
          StringSet.mem x vars
          || StringSet.mem x spread_vars
          || StringMap.mem x param_vars
        then raise (KindError ("Existential " ^ x ^ " shadows another name"))
        else StringSet.add x vars)
      vars sg.exists
  in
  let ctx = (vars, spread_vars, param_vars) in
  List.iter (check_constr_signature ctx) sg.ensures;

  let rec check_ret = function
    | TypeInt | TypeLiteralInt _ -> ()
    | IntExpr e -> check_arith_signature ctx e
    | TypeTuple ts -> List.iter check_ret ts
    | Nparray l ->
        ignore
          (List.fold_left
             (fun acc e -> check_entry_signature acc e false)
             ctx l)
  in
  check_ret sg.ret

(* the value of an integer parameter's argument as a Z3 expression, if known *)
let int_param_expr (x : string) (param_mapping : arg StringMap.t) :
    Z3.Expr.expr option =
  match StringMap.find_opt x param_mapping with
  | Some (LiteralInt i) -> Some (mk_int_numeral i)
  | Some (SymInt v) -> Some (Z3utils.mk_int v)
  | _ -> None

let get_concrete_indices (indices : (string, int) either list)
    (param_mapping : arg StringMap.t) : int list =
  List.map
    (fun index ->
      match index with
      | Left s ->
          (* a symbolic int is fine as long as the constraints pin it down *)
          begin match
            Option.bind (int_param_expr s param_mapping) Z3utils.determined_int
          with
          | Some i -> i
          | None ->
              raise
                (TypeError ("Index " ^ s ^ " must have a statically known value"))
          end
      | Right asdf -> asdf)
    indices

(* the position python index i refers to in items, provided no list variable
   makes it ambiguous. for insertion, the list is one longer *)
let resolve_index ?(insert = false) (items : string list) (i : int) : int option
    =
  let len = List.length items + if insert then 1 else 0 in
  let p = Utils.normalize_index len i in
  (* positions whose lengths the index depends on: before it for i >= 0,
     from it to the end for i < 0 *)
  let depends j = if i >= 0 then j < p || ((not insert) && j = p) else j >= p in
  if p < 0 || p >= len then None
  else if
    List.exists Fun.id
      (List.mapi (fun j x -> depends j && Z3utils.is_list_var x) items)
  then None
  else Some p

let resolve_indices (items : string list) (indices : int list) : int list option
    =
  List.fold_right
    (fun i acc ->
      Option.bind acc (fun acc ->
          Option.map (fun p -> p :: acc) (resolve_index items i)))
    indices (Some [])

type dim_error =
  | Unknown_param of string (* an integer parameter with no known value *)
  | Bad_divisor of entry (* a divisor not provably positive *)
  | Bad_index of entry * string list (* A[i] out of range or ambiguous *)

let show_dim_error = function
  | Unknown_param x -> "integer parameter " ^ x ^ " has no known value"
  | Bad_divisor e -> "divisor " ^ string_of_entry e ^ " may not be positive"
  | Bad_index (e, dims) ->
      string_of_entry e ^ " is undefined for " ^ string_of_dims dims

(* translate an arithmetic dimension into a Z3 expression *)
let rec expr_of_dim (s : entry)
    ((var_mapping, spread_mapping, param_mapping) as mapping : mapping_type) :
    (Z3.Expr.expr, dim_error) result =
  let ( let* ) = Result.bind in
  let binop mk e1 e2 =
    let* l = expr_of_dim e1 mapping in
    let* r = expr_of_dim e2 mapping in
    Ok (mk l r)
  in
  match s with
  | Id x -> (
      match StringMap.find_opt x var_mapping with
      | Some e -> Ok e
      | None ->
          Option.to_result ~none:(Unknown_param x)
            (int_param_expr x param_mapping))
  | Int i -> Ok (mk_int_numeral i)
  | Add (s1, s2) ->
      binop (fun l r -> Z3.Arithmetic.mk_add Z3utils.ctx [ l; r ]) s1 s2
  | Sub (s1, s2) ->
      binop (fun l r -> Z3.Arithmetic.mk_sub Z3utils.ctx [ l; r ]) s1 s2
  | Mul (s1, s2) ->
      binop (fun l r -> Z3.Arithmetic.mk_mul Z3utils.ctx [ l; r ]) s1 s2
  | Div (s1, s2) ->
      let* l = expr_of_dim s1 mapping in
      let* r = expr_of_dim s2 mapping in
      (* z3 integer division is floor division for positive divisors *)
      if Z3utils.prove (Z3.Arithmetic.mk_gt Z3utils.ctx r (mk_int_numeral 0))
      then Ok (Z3.Arithmetic.mk_div Z3utils.ctx l r)
      else Error (Bad_divisor s2)
  | Prod a -> (
      let factor x =
        if Z3utils.is_list_var x then Z3utils.prod_of_list x
        else Z3utils.mk_int x
      in
      match
        List.map factor (Z3utils.expand (StringMap.find a spread_mapping))
      with
      | [] -> Ok (mk_int_numeral 1)
      | [ d ] -> Ok d
      | ds -> Ok (Z3.Arithmetic.mk_mul Z3utils.ctx ds))
  | Index (a, i) -> (
      let items = Z3utils.expand (StringMap.find a spread_mapping) in
      match
        Option.bind
          (resolve_index items
             (List.hd (get_concrete_indices [ i ] param_mapping)))
          (fun p ->
            let d = List.nth items p in
            if Z3utils.is_list_var d then None else Some d)
      with
      | Some d -> Ok (Z3utils.mk_int d)
      | None -> Error (Bad_index (s, items)))
  | Rank a ->
      let lists, dims =
        List.partition Z3utils.is_list_var
          (Z3utils.expand (StringMap.find a spread_mapping))
      in
      if lists = [] then Ok (mk_int_numeral (List.length dims))
      else
        Ok
          (Z3.Arithmetic.mk_add Z3utils.ctx
             (mk_int_numeral (List.length dims)
             :: List.map Z3utils.rank_of_list lists))
  | _ -> raise (TypeError "Called with wrong argument")

let all_ones (l : string list) =
  List.for_all
    (fun x ->
      (not (Z3utils.is_list_var x))
      && Z3utils.prove_int_eq (Z3utils.mk_int x) (mk_int_numeral 1))
    l

(* the number of dims in l, as a solver term *)
let length_expr (l : string list) : Z3.Expr.expr =
  let lists, dims = List.partition Z3utils.is_list_var l in
  Z3.Arithmetic.mk_add Z3utils.ctx
    (mk_int_numeral (List.length dims) :: List.map Z3utils.rank_of_list lists)

(* right-aligned numpy broadcasting of two dimension lists. each aligned pair
   must provably be equal or have a 1; the result is ite(x = 1, y, x) *)
let broadcast_pair (a : string list) (b : string list) : string list option =
  let one = mk_int_numeral 1 in
  let rec go ra rb acc =
    (* the rest of long broadcasts against 1s and is unchanged, if long
       provably has at least as many dims *)
    let absorbs long ones =
      all_ones ones
      && Z3utils.prove
           (Z3.Arithmetic.mk_ge Z3utils.ctx (length_expr long)
              (mk_int_numeral (List.length ones)))
    in
    match (ra, rb) with
    | [], rest | rest, [] -> Some (List.rev_append rest acc)
    (* otherwise a list variable only broadcasts with itself: anything else
       would depend on its unknown length *)
    | x :: ra', y :: rb' when Z3utils.is_list_var x || Z3utils.is_list_var y ->
        if x = y then go ra' rb' (x :: acc)
        else if absorbs ra rb then Some (List.rev_append ra acc)
        else if absorbs rb ra then Some (List.rev_append rb acc)
        else None
    | x :: ra', y :: rb' ->
        let ex, ey = (Z3utils.mk_int x, Z3utils.mk_int y) in
        let eq = Z3.Boolean.mk_eq Z3utils.ctx in
        if
          not
            (Z3utils.prove
               (Z3.Boolean.mk_or Z3utils.ctx [ eq ex ey; eq ex one; eq ey one ]))
        then None
        else
          let r =
            if Z3utils.prove_int_eq ex ey || Z3utils.prove_int_eq ey one then x
            else if Z3utils.prove_int_eq ex one then y
            else
              Z3utils.add_to_solver
                (Z3.Boolean.mk_ite Z3utils.ctx (eq ex one) ey ex)
          in
          go ra' rb' (r :: acc)
  in
  go (List.rev a) (List.rev b) []

(* list-valued dimensions computed from already-bound spreads. None means
   the computation is undefined for these spreads *)
let derived_dims (e : entry)
    ((_, spread_mapping, param_mapping) as mapping : mapping_type) :
    string list option =
  let spread v = Z3utils.expand (StringMap.find v spread_mapping) in
  (* a new dimension variable for an arithmetic entry, if provably natural *)
  let new_dim d =
    match expr_of_dim d mapping with
    | Ok e
      when Z3utils.prove (Z3.Arithmetic.mk_ge Z3utils.ctx e (mk_int_numeral 0))
      ->
        Some (Z3utils.add_to_solver e)
    | _ -> None
  in
  (* apply f to v's dims and the resolved positions of indices *)
  let at v indices f =
    let items = spread v in
    Option.bind
      (resolve_indices items (get_concrete_indices indices param_mapping))
      (f items)
  in
  match e with
  | Drop (v, indices) -> at v indices Utils.drop
  | Keep (v, indices) -> at v indices Utils.keep
  | Broadcasted names -> (
      match List.map spread names with
      | [] -> None
      | first :: rest ->
          List.fold_left
            (fun acc l -> Option.bind acc (fun acc -> broadcast_pair acc l))
            (Some first) rest)
  | Permute (v, indices) ->
      if List.exists Z3utils.is_list_var (spread v) then None
      else at v indices Utils.permute
  | SetAt (v, indices, d) ->
      Option.bind (new_dim d) (fun d ->
          at v indices (fun items ps -> Utils.set_at items ps d))
  | InsertAt (v, index, d) ->
      Option.bind (new_dim d) (fun d ->
          let items = spread v in
          match get_concrete_indices [ index ] param_mapping with
          | [ i ] ->
              Option.bind (resolve_index ~insert:true items i) (fun p ->
                  Utils.insert_at items p d)
          | _ -> None)
  | _ -> invalid_arg "derived_dims"

(* if s2 starts with dimensions provably equal to l, return the rest *)
let strip_equal_prefix (l : string list) (s2 : string list) : string list option
    =
  let l = Z3utils.expand l and s2 = Z3utils.expand s2 in
  if List.length l > List.length s2 then None
  else
    let front, back = Utils.take_n s2 (List.length l) in
    if
      List.for_all2
        (fun x y ->
          (* list variables are only equal to themselves *)
          if Z3utils.is_list_var x || Z3utils.is_list_var y then x = y
          else Z3utils.prove_int_eq (Z3utils.mk_int x) (Z3utils.mk_int y))
        l front
    then Some back
    else None

(* the first failure in the furthest parameter reached, reported when nothing
   matches. preferring the first failure means spreads are read as capturing
   as little as possible. messages are lazy because backtracking fails often *)
type failure = { param_index : int; message : unit -> string }

let best_failure : failure option ref = ref None

(* what is being matched, e.g. "parameter X", with its type and argument *)
let current_params : (string * typ * arg) array ref = ref [||]

let fail (restfunargstyps : (string * typ) list) (message : unit -> string) :
    'a option =
  let index = Array.length !current_params - List.length restfunargstyps - 1 in
  (match !best_failure with
  | Some best when best.param_index >= index -> ()
  | _ ->
      let what, typ, arg = !current_params.(index) in
      best_failure :=
        Some
          {
            param_index = index;
            message =
              (fun () ->
                what ^ " (" ^ string_of_typ typ ^ ", given " ^ string_of_arg arg
                ^ "): " ^ message ());
          });
  None

(* the spreads a derived dimension reads, with their values *)
let explain_spreads (e : entry) ((_, spread_mapping, _) : mapping_type) : string
    =
  let names =
    match e with
    | Drop (a, _)
    | Keep (a, _)
    | Permute (a, _)
    | SetAt (a, _, _)
    | InsertAt (a, _, _)
    | Spread a
    | Broadcast a
    | Prod a
    | Rank a ->
        [ a ]
    | Broadcasted l -> l
    | _ -> []
  in
  String.concat ", "
    (List.map
       (fun a ->
         a ^ " = "
         ^
         match StringMap.find_opt a spread_mapping with
         | Some l -> string_of_dims l
         | None -> "?")
       names)

(* unfold a list variable for the rest of the match, undoing it if that fails *)
let with_unfolding ~(right : bool) (x : string) (k : unit -> 'a option) :
    'a option =
  let saved = !Z3utils.unfoldings in
  Z3utils.unfold ~right x;
  match k () with
  | Some r -> Some r
  | None ->
      Z3utils.unfoldings := saved;
      None

let rec check_app' (funargtyps : (string * typ) list) (argtyps : arg list)
    (mappings : mapping_type) : mapping_type option =
  match (funargtyps, argtyps) with
  | [], [] -> Some mappings
  | (_, funargtyp) :: restfunargtyps, argtyp :: restargtyps ->
      check_and_update_mapping funargtyp argtyp mappings restfunargtyps
        restargtyps
  | _ -> failwith "impossible"

and check_and_update_mapping (curr_typ : typ) (l2 : arg)
    (mapping : mapping_type) (restfunargstyps : (string * typ) list)
    (restargtyps : arg list) : mapping_type option =
  let mismatch () =
    fail restfunargstyps (fun () ->
        match curr_typ with
        | Nparray _ -> "expected an array"
        | _ -> "expected an int")
  in
  match (curr_typ, l2) with
  (* the type is int and an int was provided *)
  | TypeInt, (LiteralInt _ | Int | SymInt _ | Dimensions [])
  | Nparray [], (LiteralInt _ | Int | SymInt _) ->
      check_app' restfunargstyps restargtyps mapping
  | TypeLiteralInt i, LiteralInt j when i = j ->
      check_app' restfunargstyps restargtyps mapping
  | TypeLiteralInt i, SymInt v
    when Z3utils.prove_int_eq (Z3utils.mk_int v) (mk_int_numeral i) ->
      check_app' restfunargstyps restargtyps mapping
  (* the int must provably equal the expression *)
  | IntExpr e, (LiteralInt _ | SymInt _) ->
      let value =
        match l2 with
        | LiteralInt i -> mk_int_numeral i
        | SymInt v -> Z3utils.mk_int v
        | _ -> failwith "impossible"
      in
      begin match expr_of_dim e mapping with
      | Ok expected when Z3utils.prove_int_eq expected value ->
          check_app' restfunargstyps restargtyps mapping
      | Ok expected ->
          fail restfunargstyps (fun () ->
              "expected an int equal to "
              ^ with_value (string_of_entry e) expected
              ^ ", got " ^ string_of_expr value)
      | Error err -> fail restfunargstyps (fun () -> show_dim_error err)
      end
  (* the type is nparray and list of dimensions was provided *)
  | Nparray l1, Dimensions l2 ->
      let attempt l2 =
        match (l1, l2) with
        | [], [] -> check_app' restfunargstyps restargtyps mapping
        | [], extra ->
            fail restfunargstyps (fun () ->
                Printf.sprintf
                  "%d more dimension(s) than the signature allows: %s"
                  (List.length extra) (string_of_dims extra))
        (* even with no dimensions left, h may be a spread that captures [] *)
        | h :: t, args ->
            check_and_update_individual_mapping h args mapping restfunargstyps
              restargtyps t
      in
      (* if nothing matches, a list variable provably nonempty may be split
         into [*B', b] to expose its last dim, e.g. for batched matmul *)
      let rec with_unfoldings l2 =
        let l2 = Z3utils.expand l2 in
        match attempt l2 with
        | Some m -> Some m
        | None ->
            List.find_map
              (fun x ->
                with_unfolding ~right:true x (fun () -> with_unfoldings l2))
              (List.filter Z3utils.can_unfold l2)
      in
      with_unfoldings l2
  (* no other pairing is well-typed *)
  | _, _ -> mismatch ()

and check_and_update_individual_mapping (s1 : entry) (* the signature's type *)
    (s2 : string list) (* the remaining things in the argument *)
    ((var_mapping, spread_mapping, param_mapping) as mapping : mapping_type)
    (* the mappings thus far *) (restfunargstyps : (string * typ) list)
    (restargtyps : arg list) (restentries : entry list) : mapping_type option =
  let fail_here message = fail restfunargstyps message in
  let too_few () =
    fail_here (fun () -> "ran out of dimensions matching " ^ string_of_entry s1)
  in
  let expected_got (expected : unit -> string) (got : string) =
    fail_here (fun () ->
        "expected " ^ expected () ^ ", got " ^ string_of_dim got)
  in
  let continue_with t mapping =
    check_and_update_mapping (Nparray restentries) (Dimensions t) mapping
      restfunargstyps restargtyps
  in
  match s1 with
  | (Id _ | Int _ | Add _ | Sub _ | Mul _ | Div _ | Prod _ | Rank _ | Index _)
    when match s2 with h :: _ -> Z3utils.is_list_var h | [] -> false ->
      let h = List.hd s2 in
      let unknown () =
        fail_here (fun () ->
            "can't match " ^ string_of_entry s1 ^ " against " ^ string_of_dim h
            ^ ", which has an unknown number of dimensions")
      in
      (* a provably nonempty list variable can give up its first dim *)
      if Z3utils.can_unfold h then
        match
          with_unfolding ~right:false h (fun () ->
              check_and_update_individual_mapping s1 (Z3utils.expand s2) mapping
                restfunargstyps restargtyps restentries)
        with
        | Some m -> Some m
        | None -> unknown ()
      else unknown ()
  | Id x
    when (not (StringMap.mem x var_mapping)) && StringMap.mem x param_mapping ->
      (* x names an integer parameter: the dimension must equal its value *)
      begin match (s2, int_param_expr x param_mapping) with
      | [], _ -> too_few ()
      | h :: t, Some v when Z3utils.prove_int_eq v (Z3utils.mk_int h) ->
          continue_with t mapping
      | h :: _, Some v -> expected_got (fun () -> with_value x v) h
      | h :: _, None ->
          expected_got (fun () -> x ^ " (an int of unknown value)") h
      end
  | Id x ->
      begin match s2 with
      | [] -> too_few ()
      | h :: t ->
          (* there are args left, take the first one *)
          begin match StringMap.find_opt x var_mapping with
          | None ->
              (* if variable is not yet mapped, store a new mapping and continue typechecking *)
              let new_var_mapping =
                StringMap.add x (Z3utils.mk_int h) var_mapping
              in
              continue_with t (new_var_mapping, spread_mapping, param_mapping)
          | Some exp ->
              (* if we mapped variable already, then try to prove it's equal to what's already stored *)
              if Z3utils.prove_int_eq exp (Z3utils.mk_int h) then
                continue_with t mapping
              else expected_got (fun () -> with_value x exp) h
          end
      end
  | Add _ | Sub _ | Mul _ | Div _ | Prod _ | Rank _ | Index _ ->
      begin match s2 with
      | [] -> too_few ()
      | h :: t -> (
          (* there are args left, try to prove the equality *)
          match expr_of_dim s1 mapping with
          | Ok expr_e when Z3utils.prove_int_eq expr_e (Z3utils.mk_int h) ->
              continue_with t mapping
          | Ok expr_e ->
              expected_got (fun () -> with_value (string_of_entry s1) expr_e) h
          | Error err -> fail_here (fun () -> show_dim_error err))
      end
  | Spread v ->
      (* spread operator, capturing 0 or more variables *)
      begin match StringMap.find_opt v spread_mapping with
      | None ->
          (* haven't mapped this spread variable yet *)
          let split_rem = Utils.all_splits s2 in

          let rec try_splits (splits : (string list * string list) list) =
            begin match splits with
            | [] -> None
            | (front, back) :: t ->
                let new_spread_mapping = StringMap.add v front spread_mapping in
                let mapping_attempt =
                  (* attempt to continue the typechecking with this mapping *)
                  continue_with back
                    (var_mapping, new_spread_mapping, param_mapping)
                in

                begin match mapping_attempt with
                | None ->
                    try_splits
                      t (* that mapping failed, try the next one instead *)
                | Some mapping ->
                    Some mapping (* the mapping succeeded through the end! *)
                end
            end
          in

          try_splits split_rem
      | Some l ->
          (* we've already mapped the spread variable to a list of vars;
             check the next dimensions are provably equal to them *)
          begin match strip_equal_prefix l s2 with
          | Some back -> continue_with back mapping
          | None ->
              fail_here (fun () ->
                  "expected *" ^ v ^ " = " ^ string_of_dims l ^ " next, got "
                  ^ string_of_dims s2)
          end
      end
  | Drop _ | Keep _ | Broadcasted _ | Permute _ | SetAt _ | InsertAt _ ->
      begin match derived_dims s1 mapping with
      | None ->
          fail_here (fun () ->
              string_of_entry s1 ^ " is undefined for "
              ^ explain_spreads s1 mapping)
      | Some l -> (
          match strip_equal_prefix l s2 with
          | Some back -> continue_with back mapping
          | None ->
              fail_here (fun () ->
                  "expected " ^ string_of_entry s1 ^ " = " ^ string_of_dims l
                  ^ " next, got " ^ string_of_dims s2))
      end
  | Int i ->
      begin match s2 with
      (* there are no more variables to even match with *)
      | [] -> too_few ()
      (* there is at least one variable to match with *)
      | h :: t ->
          (* if we can prove that the provided integer is equal to the dimension
             in the array, continue *)
          if prove_int_eq (mk_int_numeral i) (mk_int h) then
            continue_with t mapping
          (* we can't prove it, fail *)
            else expected_got (fun () -> string_of_int i) h
      end
  | Broadcast s ->
      let arr = Z3utils.expand (StringMap.find s spread_mapping) in
      let rev_arr = List.rev arr in

      let splits = Utils.all_splits s2 in

      let rec prove_broadcast l1 l2 =
        match (l1, l2) with
        | [], _ | _, [] -> true
        | h1 :: t1, h2 :: t2
          when Z3utils.is_list_var h1 || Z3utils.is_list_var h2 ->
            (* 1s broadcast with whatever a list variable turns out to be *)
            (h1 = h2 && prove_broadcast t1 t2) || all_ones l1 || all_ones l2
        | h1 :: t1, h2 :: t2 ->
            (prove_int_eq (mk_int h1) (mk_int h2)
            || prove_int_eq (mk_int h1) (mk_int_numeral 1)
            || prove_int_eq (mk_int h2) (mk_int_numeral 1))
            && prove_broadcast t1 t2
      in

      let rec try_splits l =
        match l with
        | [] ->
            fail_here (fun () ->
                "no prefix of " ^ string_of_dims s2 ^ " broadcasts with *" ^ s
                ^ " = " ^ string_of_dims arr)
        | (front, back) :: t ->
            let rev = List.rev front in
            if prove_broadcast rev rev_arr then
              begin match continue_with back mapping with
              | None -> try_splits t
              | Some x -> Some x
              end
            else try_splits t
      in

      try_splits splits

(* ---- inferred preconditions ---- *)

(* while checking a body with inference on, it may assume sign facts about its
   parameters when it can't prove a size is valid: an int parameter is >= 0 or
   >= 1, and a dim is >= 1. each fact it assumes becomes a requires that
   callers prove. torch raises when these facts fail, e.g. torch.ones(-1).
   only size obligations use it: an int used as a shape entry or a returned
   dim is >= 0, the other sizes of a -1 have a positive product, and a
   callee's sign-fact requires hold. relations like conv2d's kernel fitting
   the image must be proved *)
type inference = {
  candidates : (Z3.Expr.expr * constr) list; (* stronger facts first *)
  mutable inferred : constr list; (* newest first *)
}

let inference : inference option ref = ref None

(* prove e, or assume the fewest candidate facts that prove it *)
let prove_or_infer (e : Z3.Expr.expr) : bool =
  Z3utils.prove e
  ||
  match !inference with
  | None -> false
  | Some inf ->
      let open_facts =
        List.filter (fun (_, c) -> not (List.mem c inf.inferred)) inf.candidates
      in
      let suffice facts =
        facts <> []
        && Z3utils.prove
             (Z3.Boolean.mk_implies Z3utils.ctx
                (Z3.Boolean.mk_and Z3utils.ctx (List.map fst facts))
                e)
      in
      suffice open_facts
      &&
      (* drop every fact the others suffice without, stronger ones first, so
         n >= 0 is kept over n >= 1 *)
      let needed =
        List.fold_left
          (fun needed f ->
            let rest = List.filter (fun g -> g != f) needed in
            if suffice rest then rest else needed)
          open_facts open_facts
      in
      List.iter
        (fun (fact, c) ->
          Z3.Solver.add Z3utils.solver [ fact ];
          inf.inferred <- c :: inf.inferred)
        needed;
      true

let without_inference (f : unit -> 'a) : 'a =
  let saved = !inference in
  inference := None;
  Fun.protect ~finally:(fun () -> inference := saved) f

(* the dims standing for a -1 in a shape, e.g. x.reshape(-1, d), until a
   requires equation of the callee determines them *)
let pending_sizes : StringSet.t ref = ref StringSet.empty

(* run f, undoing its assertions and bookkeeping if it fails. on success the
   solver keeps f's scope, which the enclosing Z3utils.scoped pops *)
let attempt (f : unit -> 'a) : 'a =
  let unfoldings = !Z3utils.unfoldings and pending = !pending_sizes in
  let inferred = Option.map (fun inf -> inf.inferred) !inference in
  Z3.Solver.push Z3utils.solver;
  try f ()
  with e ->
    Z3.Solver.pop Z3utils.solver 1;
    Z3utils.unfoldings := unfoldings;
    pending_sizes := pending;
    (match (!inference, inferred) with
    | Some inf, Some l -> inf.inferred <- l
    | _ -> ());
    raise e

let rec mentions (x : string) (e : Z3.Expr.expr) : bool =
  (Z3.Expr.get_num_args e = 0 && Z3.Expr.to_string e = x)
  || List.exists (mentions x) (Z3.Expr.get_args e)

let rec factors (e : Z3.Expr.expr) : Z3.Expr.expr list =
  if Z3.Arithmetic.is_mul e then List.concat_map factors (Z3.Expr.get_args e)
  else [ e ]

let product (es : Z3.Expr.expr list) : Z3.Expr.expr =
  match es with
  | [] -> mk_int_numeral 1
  | [ e ] -> e
  | es -> Z3.Arithmetic.mk_mul Z3utils.ctx es

(* total / rest by cancelling factors, e.g. b * n * 12 / (b * 3 * 4) = n:
   each factor of rest provably equals one of total's, or the unmatched ones
   are numbers dividing the product of total's numbers. nonlinear divisibility
   is often too hard for the solver, and this is the common case *)
let quotient (total : Z3.Expr.expr) (rest : Z3.Expr.expr) : Z3.Expr.expr option
    =
  let rec cancel ts unmatched = function
    | [] -> (ts, unmatched)
    | f :: fs -> (
        match List.find_opt (Z3utils.prove_int_eq f) ts with
        | Some g -> cancel (List.filter (fun t -> t != g) ts) unmatched fs
        | None -> cancel ts (f :: unmatched) fs)
  in
  let ts, unmatched = cancel (factors total) [] (factors rest) in
  let values es = List.map Z3utils.determined_int es in
  match values unmatched with
  | vs when List.for_all Option.is_some vs ->
      let divisor = List.fold_left (fun a v -> a * Option.get v) 1 vs in
      let numbers, others =
        List.partition (fun t -> Z3utils.determined_int t <> None) ts
      in
      let n = List.fold_left (fun a v -> a * Option.get v) 1 (values numbers) in
      if divisor <> 0 && n mod divisor = 0 then
        let k = n / divisor in
        Some (product (if k = 1 then others else mk_int_numeral k :: others))
      else None
  | _ -> None

(* determine the pending size x from total = product, where product is x times
   the other sizes: torch needs their product to be positive and to divide
   the total *)
let infer_size (x : string) (total : Z3.Expr.expr) (product : Z3.Expr.expr) :
    unit =
  let vx = Z3utils.mk_int x in
  let rest =
    Z3.Expr.simplify (Z3.Expr.substitute_one product vx (mk_int_numeral 1)) None
  in
  let times_rest = Z3.Arithmetic.mk_mul Z3utils.ctx [ vx; rest ] in
  if Z3utils.prove (Z3.Boolean.mk_eq Z3utils.ctx product times_rest) then begin
    if
      not
        (prove_or_infer
           (Z3.Arithmetic.mk_gt Z3utils.ctx rest (mk_int_numeral 0)))
    then
      raise
        (TypeError
           ("can't infer the size -1: the other sizes' product "
          ^ string_of_expr rest ^ " may be 0"));
    let q = quotient total rest in
    let definition =
      match q with
      | Some q -> Z3.Boolean.mk_eq Z3utils.ctx vx q
      | None ->
          if
            not
              (Z3utils.prove
                 (Z3.Boolean.mk_eq Z3utils.ctx
                    (Z3.Arithmetic.Integer.mk_mod Z3utils.ctx total rest)
                    (mk_int_numeral 0)))
          then
            raise
              (TypeError
                 ("can't infer the size -1: " ^ string_of_expr total
                ^ " may not be divisible by " ^ string_of_expr rest));
          Z3.Boolean.mk_eq Z3utils.ctx times_rest total
    in
    Z3.Solver.add Z3utils.solver [ definition ];
    Hashtbl.replace Z3utils.dim_labels x
      (match q with
      | Some q -> string_of_expr q
      | None -> string_of_expr total ^ " // " ^ string_of_expr rest);
    pending_sizes := StringSet.remove x !pending_sizes
  end

let rec check_ret_type_with_mapping (rettyp : typ)
    ((var_mapping, spread_mapping, param_mapping) as mapping : mapping_type) :
    arg =
  match rettyp with
  | TypeInt -> Int
  | TypeLiteralInt i -> LiteralInt i
  | TypeTuple ts ->
      Tuple (List.map (fun t -> check_ret_type_with_mapping t mapping) ts)
  | IntExpr e -> (
      match expr_of_dim e mapping with
      | Ok e ->
          let name = Z3utils.mk_string () in
          Z3.Solver.add Z3utils.solver
            [ Z3.Boolean.mk_eq Z3utils.ctx (Z3utils.mk_int name) e ];
          (* labeled by its value, e.g. d for x.size(-1) *)
          Hashtbl.replace Z3utils.dim_labels name (string_of_expr e);
          SymInt name
      | Error (Unknown_param _) -> Int
      | Error err -> raise (TypeError (show_dim_error err)))
  | Nparray l ->
      let rec check_ret_type_with_mapping' (l : entry list) : string list =
        match l with
        | [] -> []
        | h :: t ->
            begin match h with
            | Id _ | Add _ | Sub _ | Mul _ | Div _ | Prod _ | Rank _ | Index _
              ->
                let bound_name =
                  match expr_of_dim h mapping with
                  | Ok e ->
                      (* returned dimensions must be natural numbers *)
                      if
                        not
                          (prove_or_infer
                             (Z3.Arithmetic.mk_ge Z3utils.ctx e
                                (mk_int_numeral 0)))
                      then
                        raise
                          (TypeError
                             ("Returned dimension "
                             ^ with_value (string_of_entry h) e
                             ^ " may be negative"));
                      Z3utils.add_to_solver e
                  (* an integer parameter of unknown value yields a fresh,
                     unconstrained dimension *)
                  | Error (Unknown_param _) -> Z3utils.fresh_dim ()
                  | Error err -> raise (TypeError (show_dim_error err))
                in
                bound_name :: check_ret_type_with_mapping' t
            | Spread v ->
                let args = Z3utils.expand (StringMap.find v spread_mapping) in
                args @ check_ret_type_with_mapping' t
            | Drop _ | Keep _ | Broadcasted _ | Permute _ | SetAt _ | InsertAt _
              ->
                begin match derived_dims h mapping with
                | None ->
                    raise
                      (TypeError
                         ("Cannot compute " ^ string_of_entry h ^ " for "
                        ^ explain_spreads h mapping))
                | Some res -> res @ check_ret_type_with_mapping' t
                end
            | Int i ->
                let gend_var = mk_int_var i in

                gend_var :: check_ret_type_with_mapping' t
            | Broadcast _ -> raise (TypeError "Broadcast in return type")
            end
      in

      Dimensions (check_ret_type_with_mapping' l)

let constr_expr (c : constr) (mapping : mapping_type) :
    (Z3.Expr.expr, dim_error) result =
  let ( let* ) = Result.bind in
  let rel mk e1 e2 =
    let* l = expr_of_dim e1 mapping in
    let* r = expr_of_dim e2 mapping in
    Ok (mk Z3utils.ctx l r)
  in
  match c with
  | Eq (e1, e2) -> rel Z3.Boolean.mk_eq e1 e2
  | Le (e1, e2) -> rel Z3.Arithmetic.mk_le e1 e2
  | Lt (e1, e2) -> rel Z3.Arithmetic.mk_lt e1 e2

let string_of_constr (c : constr) (mapping : mapping_type) : string =
  let side e =
    match expr_of_dim e mapping with
    | Ok v -> with_value (string_of_entry e) v
    | Error _ -> string_of_entry e
  in
  let op, e1, e2 =
    match c with
    | Eq (a, b) -> ("=", a, b)
    (* a lower bound reads as n >= 0 *)
    | Le ((Int _ as a), b) -> (">=", b, a)
    | Lt ((Int _ as a), b) -> (">", b, a)
    | Le (a, b) -> ("<=", a, b)
    | Lt (a, b) -> ("<", a, b)
  in
  side e1 ^ " " ^ op ^ " " ^ side e2

(* how far the last check_sig got: the index of the parameter that failed, the
   number of parameters if they all matched, or -1 for the wrong arity *)
let sig_progress = ref 0

let check_sig (sg : signature) (argtyps : arg list) : arg =
  check_signature sg;
  if List.length sg.params <> List.length argtyps then (
    sig_progress := -1;
    raise (TypeError "Incorrect number of arguments to function"))
  else
    let param_mapping =
      List.fold_left2
        (fun map (param_name, _) arg -> StringMap.add param_name arg map)
        StringMap.empty sg.params argtyps
    in

    List.iter
      (function
        | Dimensions l ->
            List.iter
              (fun d ->
                if not (Z3utils.is_list_var d) then Z3utils.assume_dim d)
              l
        | _ -> ())
      argtyps;

    best_failure := None;
    current_params :=
      Array.of_list
        (List.map2
           (fun (name, typ) arg -> ("parameter " ^ name, typ, arg))
           sg.params argtyps);
    let final_mapping =
      check_app' sg.params argtyps
        (StringMap.empty, StringMap.empty, param_mapping)
    in
    match final_mapping with
    | None ->
        sig_progress :=
          Option.fold ~none:0 ~some:(fun f -> f.param_index) !best_failure;
        raise
          (TypeError
             (match !best_failure with
             | Some f -> "Could not type check: " ^ f.message ()
             | None -> "Could not type check"))
    | Some mapping ->
        sig_progress := List.length sg.params;
        (* a -1 in an argument's shape is determined by an equation the
           callee requires, e.g. reshape's prod(A) = prod(B) *)
        let pending =
          List.concat_map
            (function
              | Dimensions l ->
                  List.filter (fun d -> StringSet.mem d !pending_sizes) l
              | _ -> [])
            argtyps
        in
        if pending <> [] then begin
          List.iter
            (fun c ->
              match c with
              | Eq (e1, e2) -> (
                  match (expr_of_dim e1 mapping, expr_of_dim e2 mapping) with
                  | Ok l, Ok r ->
                      List.iter
                        (fun x ->
                          if StringSet.mem x !pending_sizes then
                            match (mentions x l, mentions x r) with
                            | false, true -> infer_size x l r
                            | true, false -> infer_size x r l
                            | _ -> ())
                        pending
                  | _ -> ())
              | _ -> ())
            sg.requires;
          if List.exists (fun x -> StringSet.mem x !pending_sizes) pending then
            raise (TypeError "the size -1 can't be inferred here")
        end;
        (* a sign fact, like an inferred requires, may be inferred in turn;
           a relation must be proved *)
        let inferable = function
          | Le (Int (0 | 1), Id _) | Lt (Int 0, Id _) -> true
          | _ -> false
        in
        List.iter
          (fun c ->
            match constr_expr c mapping with
            | Ok e
              when (if inferable c then prove_or_infer else Z3utils.prove) e ->
                ()
            | Ok _ ->
                raise
                  (TypeError
                     ("Precondition not provable: " ^ string_of_constr c mapping))
            | Error err -> raise (TypeError (show_dim_error err)))
          sg.requires;

        let var_mapping, spread_mapping, param_mapping = mapping in
        let var_mapping =
          List.fold_left
            (fun m x ->
              StringMap.add x (Z3utils.mk_int (Z3utils.fresh_dim ())) m)
            var_mapping sg.exists
        in
        let mapping = (var_mapping, spread_mapping, param_mapping) in
        List.iter
          (fun c ->
            match constr_expr c mapping with
            | Ok e -> Z3.Solver.add Z3utils.solver [ e ]
            | Error err -> raise (TypeError (show_dim_error err)))
          sg.ensures;

        check_ret_type_with_mapping sg.ret mapping

let sig_of_funtyp ((params, ret) : funtyp) : signature =
  { params; ret; requires = []; exists = []; ensures = [] }

let check_app (f : funtyp) (argtyps : arg list) : arg =
  check_sig (sig_of_funtyp f) argtyps

(* the first overload whose signature accepts the arguments, like @overload *)
let check_overload_sigs (overloads : signature list) (argtyps : arg list) : arg
    =
  let rec go = function
    | [] -> []
    | sg :: rest -> (
        match attempt (fun () -> check_sig sg argtyps) with
        | res -> [ Ok res ]
        | exception TypeError msg ->
            (* before go rest, which overwrites it *)
            let progress = !sig_progress in
            Error (progress, msg) :: go rest)
  in
  (* prefer an overload that needs no inferred preconditions *)
  let attempts = without_inference (fun () -> go overloads) in
  let inferred =
    match List.rev attempts with
    | Ok _ :: _ -> attempts
    | _ when !inference <> None -> go overloads
    | _ -> attempts
  in
  match List.rev inferred with
  | Ok res :: _ -> res
  | _ ->
      (* list the overloads that matched the most parameters *)
      let errors =
        List.filter_map (function Error e -> Some e | Ok _ -> None) attempts
      in
      let best = List.fold_left (fun m (p, _) -> max m p) min_int errors in
      let hidden = List.length (List.filter (fun (p, _) -> p < best) errors) in
      raise
        (TypeError
           ("No overload matches the arguments:"
           ^ String.concat ""
               (List.mapi
                  (fun i (p, msg) ->
                    if p = best then
                      Printf.sprintf "\n  overload %d: %s" (i + 1) msg
                    else "")
                  errors)
           ^
           if hidden = 0 then ""
           else
             Printf.sprintf
               "\n  (%d other overload%s failed at an earlier parameter)" hidden
               (if hidden = 1 then "" else "s")))

let check_overloads (overloads : funtyp list) (argtyps : arg list) : arg =
  check_overload_sigs (List.map sig_of_funtyp overloads) argtyps

(* an array whose shape is entirely unknown, e.g. the result of an unannotated
   library call (Any): a single list variable *)
let unknown_shape () : arg = Dimensions [ Z3utils.fresh_list ~label:"?" () ]

(* ---- function bodies ---- *)

(* a straight-line function body. values are arrays and ints, so a body is a
   sequence of calls to functions whose signatures are known *)
type term =
  | Var of string
  | Lit of int
  | Call of string * term list
  | Shape of term list (* ints used as a shape, e.g. reshape(x, (n, d)) *)
  | Scalar (* a float, which broadcasts like a 0-d array *)
  | Tup of term list (* a tuple, e.g. return a, b *)
[@@deriving show]

type stmt =
  | Let of string * term (* y = f(x) *)
  | LetAnnot of string * typ * term (* y: T = f(x), checking the value is a T *)
  | Return of term
  | Unpack of string list * term (* a, b = f(x) *)
  | At of int * string * stmt (* a statement with its source line and text *)
[@@deriving show]

type callee = Sig of signature | Overloads of signature list
type fundef = { name : string; sg : signature; body : stmt list }

let rec string_of_term = function
  | Var x -> x
  | Lit i -> string_of_int i
  | Call (f, args) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_term args) ^ ")"
  | Shape ts -> "(" ^ String.concat ", " (List.map string_of_term ts) ^ ")"
  | Scalar -> "<float>"
  | Tup ts -> "(" ^ String.concat ", " (List.map string_of_term ts) ^ ")"

let rec string_of_stmt = function
  | Let (x, t) -> x ^ " = " ^ string_of_term t
  | LetAnnot (x, typ, t) ->
      x ^ ": " ^ string_of_typ typ ^ " = " ^ string_of_term t
  | Return t -> "return " ^ string_of_term t
  | Unpack (xs, t) -> String.concat ", " xs ^ " = " ^ string_of_term t
  | At (_, text, _) -> text

(* a dimension equal to an int used as a shape entry *)
let dim_of_int (v : arg) : string =
  let prove_or_infer_nonneg e =
    prove_or_infer (Z3.Arithmetic.mk_ge Z3utils.ctx e (mk_int_numeral 0))
  in
  match v with
  | LiteralInt i when i >= 0 -> Z3utils.add_to_solver (mk_int_numeral i)
  | LiteralInt i ->
      raise (TypeError ("shape entry " ^ string_of_int i ^ " is negative"))
  | SymInt v when prove_or_infer_nonneg (Z3utils.mk_int v) ->
      Z3utils.add_to_solver (Z3utils.mk_int v)
  | SymInt v ->
      raise
        (TypeError
           ("shape entry "
           ^ string_of_expr (Z3utils.mk_int v)
           ^ " may be negative"))
  (* like zeros of an opaque int: some size, unknown *)
  | Int -> Z3utils.fresh_dim ()
  | Dimensions _ | Tuple _ ->
      raise (TypeError ("expected an int in a shape, got " ^ string_of_arg v))

(* the dims of ints used as a shape. one may be -1, which a callee's requires
   determine (see check_sig) *)
let dims_of_shape (vs : arg list) : string list =
  if List.length (List.filter (( = ) (LiteralInt (-1))) vs) > 1 then
    raise (TypeError "only one size in a shape can be -1");
  List.map
    (function
      | LiteralInt -1 ->
          let d = Z3utils.fresh_dim ~label:"-1" () in
          pending_sizes := StringSet.add d !pending_sizes;
          d
      | v -> dim_of_int v)
    vs

(* a fresh int equal to e; unlike a dimension it may be negative *)
let define_int ?(label = "") (e : Z3.Expr.expr) : string =
  let name = Z3utils.mk_string () in
  Z3.Solver.add Z3utils.solver
    [ Z3.Boolean.mk_eq Z3utils.ctx (Z3utils.mk_int name) e ];
  if label <> "" then Hashtbl.replace Z3utils.dim_labels name label;
  name

(* the dims an entry of a parameter's shape stands for inside the function's
   body. its dimension and spread variables become rigid: unknowns that stand
   for whatever a caller passes, and that only ever equal themselves *)
let rigid_entry
    ((var_mapping, spread_mapping, param_mapping) as mapping : mapping_type)
    (e : entry) : string list * mapping_type =
  match e with
  | Id x
    when (not (StringMap.mem x var_mapping))
         && not (StringMap.mem x param_mapping) ->
      let d = Z3utils.fresh_dim ~label:x () in
      ( [ d ],
        ( StringMap.add x (Z3utils.mk_int d) var_mapping,
          spread_mapping,
          param_mapping ) )
  | Spread a when not (StringMap.mem a spread_mapping) ->
      let l = Z3utils.fresh_list ~label:a () in
      ([ l ], (var_mapping, StringMap.add a [ l ] spread_mapping, param_mapping))
  | Spread a -> (Z3utils.expand (StringMap.find a spread_mapping), mapping)
  | Broadcast a ->
      (* whatever broadcasts with a: nothing more is known about it *)
      ([ Z3utils.fresh_list ~label:(string_of_entry e) () ], mapping)
  | Id _ | Int _ | Add _ | Sub _ | Mul _ | Div _ | Prod _ | Rank _ | Index _
    -> (
      match expr_of_dim e mapping with
      | Ok v ->
          (* a caller's dimension equals v, so v is a natural number here *)
          let d = Z3utils.add_to_solver v in
          Hashtbl.replace Z3utils.dim_labels d (string_of_entry e);
          ([ d ], mapping)
      | Error err -> raise (TypeError (show_dim_error err)))
  | Drop _ | Keep _ | Broadcasted _ | Permute _ | SetAt _ | InsertAt _ -> (
      match derived_dims e mapping with
      | Some l -> (l, mapping)
      | None ->
          raise
            (TypeError
               ("Cannot compute " ^ string_of_entry e ^ " for "
              ^ explain_spreads e mapping)))

(* a parameter's value inside the body; later parameters and the return type
   may refer to its name *)
let rigid_param (mapping : mapping_type) ((name, typ) : string * typ) :
    arg * mapping_type =
  let arg, (var_mapping, spread_mapping, param_mapping) =
    match typ with
    | TypeInt ->
        let v = Z3utils.mk_string () in
        Hashtbl.replace Z3utils.dim_labels v name;
        (SymInt v, mapping)
    | TypeLiteralInt i -> (LiteralInt i, mapping)
    | IntExpr e -> (
        match expr_of_dim e mapping with
        | Ok v -> (SymInt (define_int ~label:name v), mapping)
        | Error err -> raise (TypeError (show_dim_error err)))
    | Nparray l ->
        let dims, mapping =
          List.fold_left
            (fun (dims, mapping) e ->
              let d, mapping = rigid_entry mapping e in
              (dims @ d, mapping))
            ([], mapping) l
        in
        (Dimensions dims, mapping)
    | TypeTuple _ -> raise (TypeError "tuple parameters aren't supported")
  in
  (arg, (var_mapping, spread_mapping, StringMap.add name arg param_mapping))

(* match a value against a type whose names are bound by mapping; unbound
   names get bound, as for a signature's exists *)
let rec match_typ (what : string) (typ : typ) (value : arg)
    (mapping : mapping_type) : mapping_type =
  match (typ, value) with
  | TypeTuple ts, Tuple vs when List.length ts = List.length vs ->
      snd
        (List.fold_left2
           (fun (i, mapping) t v ->
             (i + 1, match_typ (Printf.sprintf "%s[%d]" what i) t v mapping))
           (0, mapping) ts vs)
  | TypeTuple ts, _ ->
      raise
        (TypeError
           (Printf.sprintf "%s is %s, expected a tuple of %d" what
              (string_of_arg value) (List.length ts)))
  | _ -> match_one what typ value mapping

and match_one (what : string) (typ : typ) (value : arg) (mapping : mapping_type)
    : mapping_type =
  best_failure := None;
  current_params := [| (what, typ, value) |];
  match check_and_update_mapping typ value mapping [] [] with
  | Some mapping -> mapping
  | None ->
      raise
        (TypeError
           (match !best_failure with
           | Some f -> f.message ()
           | None -> what ^ " is not a " ^ string_of_typ typ))

(* check a function's body once, for every caller: the parameters' dimension
   and spread variables are rigid, so a body that checks is correct for every
   shape a caller could pass. callers are checked against the signature only.
   env holds the functions the body may call. with infer, the body may assume
   sign facts about the parameters, and the result is the requires they add *)
let check_body ~(infer : bool) (env : (string * callee) list) (fd : fundef) :
    constr list =
  let in_fn what f =
    try f () with
    | TypeError m -> raise (TypeError ("in " ^ fd.name ^ what ^ ": " ^ m))
    | KindError m -> raise (KindError ("in " ^ fd.name ^ what ^ ": " ^ m))
  in
  in_fn "" (fun () -> check_signature fd.sg);
  (* the body may call the function itself *)
  let env = (fd.name, Sig fd.sg) :: env in
  let rec eval locals = function
    | Var x -> (
        match StringMap.find_opt x locals with
        | Some v -> v
        | None -> raise (TypeError ("unbound variable " ^ x)))
    | Lit i -> LiteralInt i
    | Shape ts -> Dimensions (dims_of_shape (List.map (eval locals) ts))
    | Scalar -> Dimensions []
    | Tup ts -> Tuple (List.map (eval locals) ts)
    | Call (f, ts) -> (
        let args = List.map (eval locals) ts in
        try
          match List.assoc_opt f env with
          | Some (Sig sg) -> check_sig sg args
          | Some (Overloads sgs) -> check_overload_sigs sgs args
          | None -> raise (TypeError "unknown function")
        with TypeError m -> raise (TypeError (f ^ ": " ^ m)))
  in
  let constr_value mapping c =
    match constr_expr c mapping with
    | Ok e -> e
    | Error (Unknown_param x) when List.mem x fd.sg.exists ->
        raise (TypeError ("the return value doesn't determine " ^ x))
    | Error err -> raise (TypeError (show_dim_error err))
  in
  (* kinds: the names annotations may use; mapping: their values *)
  let rec walk ?at locals kinds mapping = function
    | [] -> in_fn "" (fun () -> raise (TypeError "missing return"))
    | At (line, text, stmt) :: rest ->
        walk ~at:(line, text) locals kinds mapping (stmt :: rest)
    | stmt :: rest -> (
        let label =
          match at with
          | Some (line, text) -> Printf.sprintf ", line %d, `%s`" line text
          | None -> ", `" ^ string_of_stmt stmt ^ "`"
        in
        let here f = in_fn label f in
        match stmt with
        | At _ -> assert false (* unwrapped above *)
        | Return t ->
            if rest <> [] then
              here (fun () -> raise (TypeError "statements after return"));
            let mapping =
              here (fun () ->
                  match_typ "return value" fd.sg.ret (eval locals t) mapping)
            in
            List.iter
              (fun c ->
                here (fun () ->
                    if not (Z3utils.prove (constr_value mapping c)) then
                      raise
                        (TypeError
                           ("Postcondition not provable: "
                          ^ string_of_constr c mapping))))
              fd.sg.ensures
        | Let (x, t) ->
            let v = here (fun () -> eval locals t) in
            walk (StringMap.add x v locals) kinds mapping rest
        | Unpack (xs, t) ->
            let vs =
              here (fun () ->
                  match eval locals t with
                  | Tuple vs when List.length vs = List.length xs -> vs
                  | v ->
                      raise
                        (TypeError
                           (Printf.sprintf "can't unpack %s into %d names"
                              (string_of_arg v) (List.length xs))))
            in
            let locals =
              List.fold_left2 (fun l x v -> StringMap.add x v l) locals xs vs
            in
            walk locals kinds mapping rest
        | LetAnnot (x, typ, t) ->
            let kinds =
              here (fun () ->
                  match typ with
                  | Nparray l ->
                      List.fold_left
                        (fun acc e -> check_entry_signature acc e true)
                        kinds l
                  | IntExpr e ->
                      check_arith_signature kinds e;
                      kinds
                  | TypeInt | TypeLiteralInt _ -> kinds
                  | TypeTuple _ ->
                      raise (TypeError "tuple annotations aren't supported"))
            in
            let v = here (fun () -> eval locals t) in
            let mapping =
              here (fun () -> match_typ ("annotation of " ^ x) typ v mapping)
            in
            walk (StringMap.add x v locals) kinds mapping rest)
  in
  Z3utils.scoped (fun () ->
      let args, mapping =
        in_fn "" (fun () ->
            List.fold_left
              (fun (args, mapping) p ->
                let arg, mapping = rigid_param mapping p in
                (args @ [ (fst p, arg) ], mapping))
              ([], (StringMap.empty, StringMap.empty, StringMap.empty))
              (ints_first fd.sg.params))
      in
      in_fn "" (fun () ->
          List.iter
            (fun c -> Z3.Solver.add Z3utils.solver [ constr_value mapping c ])
            fd.sg.requires;
          if Z3.Solver.check Z3utils.solver [] = Z3.Solver.UNSATISFIABLE then
            raise (TypeError "the requires clauses are contradictory"));
      let var_mapping, _, _ = mapping in
      let at_least k e = Z3.Arithmetic.mk_ge Z3utils.ctx e (mk_int_numeral k) in
      let int_params =
        List.filter_map
          (function
            | name, SymInt v
              when List.assoc_opt name fd.sg.params = Some TypeInt ->
                Some (name, Z3utils.mk_int v)
            | _ -> None)
          args
      in
      let facts k named =
        List.map (fun (x, e) -> (at_least k e, Le (Int k, Id x))) named
      in
      let candidates =
        facts 1 (StringMap.bindings var_mapping)
        @ facts 1 int_params @ facts 0 int_params
      in
      let inf = { candidates; inferred = [] } in
      if infer then inference := Some inf;
      Fun.protect
        ~finally:(fun () ->
          inference := None;
          pending_sizes := StringSet.empty)
        (fun () ->
          walk
            (StringMap.of_seq (List.to_seq args))
            (check_args_signature fd.sg.params)
            mapping fd.body);
      List.rev inf.inferred)

let check_fundef (env : (string * callee) list) (fd : fundef) : unit =
  ignore (check_body ~infer:false env fd)

(* the preconditions fd's body needs, beyond its requires *)
let infer_requires (env : (string * callee) list) (fd : fundef) : constr list =
  check_body ~infer:true env fd

(* check functions in order; each may call the ones before it *)
let check_program (env : (string * callee) list) (fds : fundef list) : unit =
  ignore
    (List.fold_left
       (fun env fd ->
         check_fundef env fd;
         (fd.name, Sig fd.sg) :: env)
       env fds)
