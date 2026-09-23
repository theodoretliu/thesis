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
[@@deriving show]

type typ =
  | Nparray of entry list
  | TypeInt
  | IntExpr of entry (* an int whose value is the given dimension expression *)
[@@deriving show]

type funtyp = (string * typ) list * typ

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
  | Some TypeInt | Some (Nparray []) | Some (IntExpr _) -> true
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
  | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Div (e1, e2) ->
      check_arith_signature (vars, _spread_vars, param_var_mapping) e1;
      check_arith_signature (vars, _spread_vars, param_var_mapping) e2
  | Spread _ | Drop _ | Keep _ | Broadcast _ | Broadcasted _ ->
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
  | Add _ | Sub _ | Mul _ | Div _ ->
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
  | Drop (arr, indices) | Keep (arr, indices) ->
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
      then raise (KindError "Arguments to drop are not integers")
      else orig
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
      | TypeInt -> (vars, spread_vars, new_param_var_mapping)
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

let check_signature ((funargtyps, rettyp) : funtyp) : unit =
  let vars, spread_vars, param_vars = check_args_signature funargtyps in

  match rettyp with
  | TypeInt -> ()
  | IntExpr e -> check_arith_signature (vars, spread_vars, param_vars) e
  | Nparray l ->
      ignore
        (List.fold_left
           (fun acc e -> check_entry_signature acc e false)
           (vars, spread_vars, param_vars)
           l)

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
let rec expr_of_dim (s : entry) (var_mapping : Z3.Expr.expr StringMap.t)
    (param_mapping : arg StringMap.t) : (Z3.Expr.expr, dim_error) result =
  let ( let* ) = Result.bind in
  let binop mk e1 e2 =
    let* l = expr_of_dim e1 var_mapping param_mapping in
    let* r = expr_of_dim e2 var_mapping param_mapping in
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
      let* l = expr_of_dim s1 var_mapping param_mapping in
      let* r = expr_of_dim s2 var_mapping param_mapping in
      (* z3 integer division is floor division for positive divisors *)
      if Z3utils.prove (Z3.Arithmetic.mk_gt Z3utils.ctx r (mk_int_numeral 0))
      then Ok (Z3.Arithmetic.mk_div Z3utils.ctx l r)
      else Error (Bad_divisor s2)
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
let derived_dims (e : entry) ((_, spread_mapping, param_mapping) : mapping_type)
    : string list option =
  let spread v = StringMap.find v spread_mapping in
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
  (* the int must provably equal the expression *)
  | IntExpr e, (LiteralInt _ | SymInt _) ->
      let var_mapping, _, param_mapping = mapping in
      let value =
        match l2 with
        | LiteralInt i -> mk_int_numeral i
        | SymInt v -> Z3utils.mk_int v
        | _ -> failwith "impossible"
      in
      begin match expr_of_dim e var_mapping param_mapping with
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
  | Add _ | Sub _ | Mul _ | Div _ ->
      begin match s2 with
      | [] -> None (* no more args left, intractable *)
      | h :: t -> (
          (* there are args left, try to prove the equality *)
          match expr_of_dim s1 var_mapping param_mapping with
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
  | Drop _ | Keep _ | Broadcasted _ ->
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
  | IntExpr e -> (
      match expr_of_dim e var_mapping param_mapping with
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
            | Id _ | Add _ | Sub _ | Mul _ | Div _ ->
                let bound_name =
                  match expr_of_dim h var_mapping param_mapping with
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
            | Drop _ | Keep _ | Broadcasted _ ->
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

let check_app ((funargtyps, rettyp) : funtyp) (argtyps : arg list) : arg =
  check_signature (funargtyps, rettyp);
  if List.length funargtyps <> List.length argtyps then
    raise (TypeError "Incorrect number of arguments to function")
  else
    let param_names = List.map fst funargtyps in

    let param_arg_pairings = List.combine param_names argtyps in

    let param_mapping =
      List.fold_left
        (fun map (param_name, arg) -> StringMap.add param_name arg map)
        StringMap.empty param_arg_pairings
    in

    List.iter
      (function Dimensions l -> List.iter Z3utils.assume_dim l | _ -> ())
      argtyps;

    let final_mapping =
      check_app' funargtyps argtyps
        (StringMap.empty, StringMap.empty, param_mapping)
    in
    match final_mapping with
    | None -> raise (TypeError "Could not type check")
    | Some mapping -> check_ret_type_with_mapping rettyp mapping
