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
[@@deriving show]

type typ =
  | Nparray of entry list
  | TypeInt
  | IntExpr of entry (* an int whose value is the given dimension expression *)
  | TypeLiteralInt of int (* python Literal[i]; bools are 0 and 1 *)
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
[@@deriving show]

exception TypeError of string
exception KindError of string

let string_of_typ = show_typ
let print_typ t = print_endline (string_of_typ t)

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
        raise (KindError (show_entry e ^ " needs an already-bound spread var"))
  | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Div (e1, e2) ->
      check_arith_signature (vars, _spread_vars, param_var_mapping) e1;
      check_arith_signature (vars, _spread_vars, param_var_mapping) e2
  | Spread _ | Drop _ | Keep _ | Broadcast _ | Broadcasted _ | Permute _
  | SetAt _ | InsertAt _ ->
      raise (KindError ("List dimension inside arithmetic: " ^ show_entry e))

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
  | Add _ | Sub _ | Mul _ | Div _ | Prod _ | Rank _ ->
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
             ("First argument to " ^ show_entry e ^ " is not a spread var"))
      else if
        List.exists
          (fun index ->
            match index with
            | Left s -> not (is_int_param s param_var_mapping)
            | Right _ -> false)
          indices
      then
        raise (KindError ("Indices to " ^ show_entry e ^ " are not integers"))
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
    funargtyps

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

  match sg.ret with
  | TypeInt | TypeLiteralInt _ -> ()
  | IntExpr e -> check_arith_signature ctx e
  | Nparray l ->
      ignore
        (List.fold_left (fun acc e -> check_entry_signature acc e false) ctx l)

(* the value of an integer parameter's argument as a Z3 expression, if known *)
let int_param_expr (x : string) (param_mapping : arg StringMap.t) :
    Z3.Expr.expr option =
  match StringMap.find_opt x param_mapping with
  | Some (LiteralInt i) -> Some (mk_int_numeral i)
  | Some (SymInt v) -> Some (Z3utils.mk_int v)
  | _ -> None

type dim_error =
  | Unknown_param of string (* an integer parameter with no known value *)
  | Bad_divisor of entry (* a divisor not provably positive *)

let show_dim_error = function
  | Unknown_param x -> "integer parameter " ^ x ^ " has no known value"
  | Bad_divisor e -> "divisor " ^ show_entry e ^ " may not be positive"

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
      match List.map Z3utils.mk_int (StringMap.find a spread_mapping) with
      | [] -> Ok (mk_int_numeral 1)
      | [ d ] -> Ok d
      | ds -> Ok (Z3.Arithmetic.mk_mul Z3utils.ctx ds))
  | Rank a ->
      Ok (mk_int_numeral (List.length (StringMap.find a spread_mapping)))
  | _ -> raise (TypeError "Called with wrong argument")

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

(* right-aligned numpy broadcasting of two dimension lists. each aligned pair
   must provably be equal or have a 1; the result is ite(x = 1, y, x) *)
let broadcast_pair (a : string list) (b : string list) : string list option =
  let one = mk_int_numeral 1 in
  let rec go ra rb acc =
    match (ra, rb) with
    | [], rest | rest, [] -> Some (List.rev_append rest acc)
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
  let spread v = StringMap.find v spread_mapping in
  (* a new dimension variable for an arithmetic entry, if provably natural *)
  let new_dim d =
    match expr_of_dim d mapping with
    | Ok e
      when Z3utils.prove (Z3.Arithmetic.mk_ge Z3utils.ctx e (mk_int_numeral 0))
      ->
        Some (Z3utils.add_to_solver e)
    | _ -> None
  in
  match e with
  | Drop (v, indices) ->
      Utils.drop (spread v) (get_concrete_indices indices param_mapping)
  | Keep (v, indices) ->
      Utils.keep (spread v) (get_concrete_indices indices param_mapping)
  | Broadcasted names -> (
      match List.map spread names with
      | [] -> None
      | first :: rest ->
          List.fold_left
            (fun acc l -> Option.bind acc (fun acc -> broadcast_pair acc l))
            (Some first) rest)
  | Permute (v, indices) ->
      Utils.permute (spread v) (get_concrete_indices indices param_mapping)
  | SetAt (v, indices, d) ->
      Option.bind (new_dim d) (fun d ->
          Utils.set_at (spread v) (get_concrete_indices indices param_mapping) d)
  | InsertAt (v, index, d) ->
      Option.bind (new_dim d) (fun d ->
          match get_concrete_indices [ index ] param_mapping with
          | [ i ] -> Utils.insert_at (spread v) i d
          | _ -> None)
  | _ -> invalid_arg "derived_dims"

(* if s2 starts with dimensions provably equal to l, return the rest *)
let strip_equal_prefix (l : string list) (s2 : string list) : string list option
    =
  if List.length l > List.length s2 then None
  else
    let front, back = Utils.take_n s2 (List.length l) in
    if
      List.for_all2
        (fun x y -> Z3utils.prove_int_eq (Z3utils.mk_int x) (Z3utils.mk_int y))
        l front
    then Some back
    else None

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
      | _ -> None
      end
  (* the type is nparray and list of dimensions was provided *)
  | Nparray l1, Dimensions l2 ->
      begin match (l1, l2) with
      | [], [] -> check_app' restfunargstyps restargtyps mapping
      | [], _ -> None
      (* even with no dimensions left, h may be a spread that captures [] *)
      | h :: t, args ->
          check_and_update_individual_mapping h args mapping restfunargstyps
            restargtyps t
      end
  (* no other pairing is well-typed *)
  | _, _ -> None

and check_and_update_individual_mapping (s1 : entry) (* the signature's type *)
    (s2 : string list) (* the remaining things in the argument *)
    ((var_mapping, spread_mapping, param_mapping) as mapping : mapping_type)
    (* the mappings thus far *) (restfunargstyps : (string * typ) list)
    (restargtyps : arg list) (restentries : entry list) : mapping_type option =
  match s1 with
  | Id x
    when (not (StringMap.mem x var_mapping)) && StringMap.mem x param_mapping ->
      (* x names an integer parameter: the dimension must equal its value *)
      begin match (s2, int_param_expr x param_mapping) with
      | h :: t, Some v when Z3utils.prove_int_eq v (Z3utils.mk_int h) ->
          check_and_update_mapping (Nparray restentries) (Dimensions t) mapping
            restfunargstyps restargtyps
      | _ -> None
      end
  | Id x ->
      begin match s2 with
      | [] -> None (* no more args left, intractable *)
      | h :: t ->
          (* there are args left, take the first one *)
          begin match StringMap.find_opt x var_mapping with
          | None ->
              (* if variable is not yet mapped, store a new mapping and continue typechecking *)
              let new_var_mapping =
                StringMap.add x (Z3utils.mk_int h) var_mapping
              in
              check_and_update_mapping (Nparray restentries) (Dimensions t)
                (new_var_mapping, spread_mapping, param_mapping)
                restfunargstyps restargtyps
          | Some exp ->
              (* if we mapped variable already, then try to prove it's equal to what's already stored *)
              if Z3utils.prove_int_eq exp (Z3utils.mk_int h) then
                check_and_update_mapping (Nparray restentries) (Dimensions t)
                  mapping restfunargstyps restargtyps
              else None
          end
      end
  | Add _ | Sub _ | Mul _ | Div _ | Prod _ | Rank _ ->
      begin match s2 with
      | [] -> None (* no more args left, intractable *)
      | h :: t -> (
          (* there are args left, try to prove the equality *)
          match expr_of_dim s1 mapping with
          | Ok expr_e when Z3utils.prove_int_eq expr_e (Z3utils.mk_int h) ->
              check_and_update_mapping (Nparray restentries) (Dimensions t)
                mapping restfunargstyps restargtyps
          | _ -> None)
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
                  check_and_update_mapping (Nparray restentries)
                    (Dimensions back)
                    (var_mapping, new_spread_mapping, param_mapping)
                    restfunargstyps restargtyps
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
          | Some back ->
              check_and_update_mapping (Nparray restentries) (Dimensions back)
                mapping restfunargstyps restargtyps
          | None -> None
          end
      end
  | Drop _ | Keep _ | Broadcasted _ | Permute _ | SetAt _ | InsertAt _ ->
      begin match
        Option.bind (derived_dims s1 mapping) (fun l -> strip_equal_prefix l s2)
      with
      | Some back ->
          check_and_update_mapping (Nparray restentries) (Dimensions back)
            mapping restfunargstyps restargtyps
      | None -> None
      end
  | Int i ->
      begin match s2 with
      (* there are no more variables to even match with *)
      | [] -> None
      (* there is at least one variable to match with *)
      | h :: t ->
          let const_int = mk_int_numeral i in
          (* if we can prove that the provided integer is equal to the dimension
             in the array, continue *)
          if prove_int_eq const_int (mk_int h) then
            check_and_update_mapping (Nparray restentries) (Dimensions t)
              mapping restfunargstyps restargtyps
          (* we can't prove it, fail *)
            else None
      end
  | Broadcast s ->
      let arr = StringMap.find s spread_mapping in
      let rev_arr = List.rev arr in

      let splits = Utils.all_splits s2 in

      let rec prove_broadcast l1 l2 =
        match (l1, l2) with
        | [], _ | _, [] -> true
        | h1 :: t1, h2 :: t2 ->
            (prove_int_eq (mk_int h1) (mk_int h2)
            || prove_int_eq (mk_int h1) (mk_int_numeral 1)
            || prove_int_eq (mk_int h2) (mk_int_numeral 1))
            && prove_broadcast t1 t2
      in

      let rec try_splits l =
        match l with
        | [] -> None
        | (front, back) :: t ->
            let rev = List.rev front in
            if prove_broadcast rev rev_arr then
              begin match
                check_and_update_mapping (Nparray restentries) (Dimensions back)
                  mapping restfunargstyps restargtyps
              with
              | None -> try_splits t
              | Some x -> Some x
              end
            else try_splits t
      in

      try_splits splits

let check_ret_type_with_mapping (rettyp : typ)
    ((var_mapping, spread_mapping, param_mapping) as mapping : mapping_type) :
    arg =
  match rettyp with
  | TypeInt -> Int
  | TypeLiteralInt i -> LiteralInt i
  | IntExpr e -> (
      match expr_of_dim e mapping with
      | Ok e ->
          let name = Z3utils.mk_string () in
          Z3.Solver.add Z3utils.solver
            [ Z3.Boolean.mk_eq Z3utils.ctx (Z3utils.mk_int name) e ];
          SymInt name
      | Error (Unknown_param _) -> Int
      | Error err -> raise (TypeError (show_dim_error err)))
  | Nparray l ->
      let rec check_ret_type_with_mapping' (l : entry list) : string list =
        match l with
        | [] -> []
        | h :: t ->
            begin match h with
            | Id _ | Add _ | Sub _ | Mul _ | Div _ | Prod _ | Rank _ ->
                let bound_name =
                  match expr_of_dim h mapping with
                  | Ok e ->
                      (* returned dimensions must be natural numbers *)
                      if
                        not
                          (Z3utils.prove
                             (Z3.Arithmetic.mk_ge Z3utils.ctx e
                                (mk_int_numeral 0)))
                      then
                        raise
                          (TypeError
                             ("Dimension " ^ show_entry h ^ " may be negative"));
                      Z3utils.add_to_solver e
                  (* an integer parameter of unknown value yields a fresh,
                     unconstrained dimension *)
                  | Error (Unknown_param _) -> Z3utils.fresh_dim ()
                  | Error err -> raise (TypeError (show_dim_error err))
                in
                bound_name :: check_ret_type_with_mapping' t
            | Spread v ->
                let args = StringMap.find v spread_mapping in
                args @ check_ret_type_with_mapping' t
            | Drop _ | Keep _ | Broadcasted _ | Permute _ | SetAt _ | InsertAt _
              ->
                begin match derived_dims h mapping with
                | None ->
                    raise
                      (TypeError
                         ("Cannot compute " ^ show_entry h
                        ^ " for these arguments"))
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

let check_sig (sg : signature) (argtyps : arg list) : arg =
  check_signature sg;
  if List.length sg.params <> List.length argtyps then
    raise (TypeError "Incorrect number of arguments to function")
  else
    let param_mapping =
      List.fold_left2
        (fun map (param_name, _) arg -> StringMap.add param_name arg map)
        StringMap.empty sg.params argtyps
    in

    List.iter
      (function Dimensions l -> List.iter Z3utils.assume_dim l | _ -> ())
      argtyps;

    let final_mapping =
      check_app' sg.params argtyps
        (StringMap.empty, StringMap.empty, param_mapping)
    in
    match final_mapping with
    | None -> raise (TypeError "Could not type check")
    | Some mapping ->
        List.iter
          (fun c ->
            match constr_expr c mapping with
            | Ok e when Z3utils.prove e -> ()
            | Ok _ ->
                raise
                  (TypeError ("Precondition not provable: " ^ show_constr c))
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

let check_app ((params, ret) : funtyp) (argtyps : arg list) : arg =
  check_sig { params; ret; requires = []; exists = []; ensures = [] } argtyps

(* the first overload whose signature accepts the arguments, like @overload *)
let check_overloads (overloads : funtyp list) (argtyps : arg list) : arg =
  let rec go = function
    | [] -> raise (TypeError "No overload matches the arguments")
    | f :: rest -> ( try check_app f argtyps with TypeError _ -> go rest)
  in
  go overloads
