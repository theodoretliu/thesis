type entry =
| Id of string
| Add of entry * entry
| Mul of entry * entry
| Spread of string
| Drop of string * string list
| Keep of string * string list
[@@deriving show]

type typ =
| Nparray of entry list
| TypeInt
[@@deriving show]

type funtyp = ((string * typ) list) * typ

type arg =
| Dimensions of string list
| LiteralInt of int
| Int
[@@deriving show]

exception TypeError of string
exception KindError of string

let string_of_typ = show_typ

let print_typ t =
  print_endline (string_of_typ t)

module StringMap = Map.Make(struct type t = string let compare = compare end)
module StringSet = Set.Make(struct type t = string let compare = compare end)

(* single dimension variables, spread variables, named parameters *)
type mapping_type = Z3.Expr.expr StringMap.t * (string list) StringMap.t * arg StringMap.t

(* given a set of already declared variables and spread variables, this
function checks that the entry type is well-kinded. particularly, it ensures
a few things:
   - new variables are not introduced under Add and Mul
   - introduction of new variables do not shadow spread variables and vice versa
   - Spread does not occur under Add or Mul
   - Drop needs a spread variable as its first argument and only vars mapping to TypeInts as its second *)
let rec check_entry_signature ((vars, spread_vars, param_var_mapping) as orig : StringSet.t * StringSet.t * typ StringMap.t)
                              (e : entry)
                              (can_intro : bool)
                            : StringSet.t * StringSet.t * typ StringMap.t =
  match e with
  | Id x ->
      if can_intro then
        if StringSet.mem x spread_vars || StringMap.mem x param_var_mapping
        then raise (KindError "Attempt to intro variable which is already spread or parameter variable")
        else StringSet.add x vars, spread_vars, param_var_mapping
      else if StringSet.mem x vars then vars, spread_vars, param_var_mapping
      else raise (KindError "Attempt to intro new variable in bad context")

  | Add (e1, e2)
  | Mul (e1, e2) ->
      begin match e1, e2 with
      | Spread _, _ | _, Spread _ -> raise (KindError "Spread inside Add")
      | _ ->
          let first_check = check_entry_signature (vars, spread_vars, param_var_mapping) e1 false in
          check_entry_signature first_check e2 false end

  | Spread x ->
      if can_intro then
        if StringSet.mem x vars || StringMap.mem x param_var_mapping
        then raise (KindError "Attempt to intro spread variable which is already variable")
        else vars, StringSet.add x spread_vars, param_var_mapping
      else if StringSet.mem x spread_vars then vars, spread_vars, param_var_mapping
      else raise (KindError "Attempt to intro new variable in bad context")

  | Drop (arr, indices)
  | Keep (arr, indices) ->
      if not (StringSet.mem arr spread_vars)
      then raise (KindError ("First argument to " ^ (show_entry e) ^ " is not a spread var"))
      else if List.exists (fun x ->
        let res = StringMap.find_opt x param_var_mapping in
        res <> Some (TypeInt) && res <> Some (Nparray [])) indices
      then raise (KindError "Arguments to drop are not integers")
      else orig



let check_args_signature (funargtyps : (string * typ) list) : StringSet.t * StringSet.t * typ StringMap.t =
  let check_args_signature' ((vars, spread_vars, param_var_mapping) : StringSet.t *  StringSet.t * typ StringMap.t)
                            ((param_name, arg) : string * typ)
                          : StringSet.t * StringSet.t * typ StringMap.t =

    (* don't allow parameter names to be in the set of already used variables *)
    if List.exists (StringSet.mem param_name) [vars; spread_vars] || StringMap.mem param_name param_var_mapping
    then raise (KindError "Parameter name already used") else

    let new_param_var_mapping = StringMap.add param_name arg param_var_mapping in

    match arg with
    | TypeInt -> vars, spread_vars, new_param_var_mapping

    | Nparray l ->
        let new_vars, new_spread_vars, new_param_var_mapping =
          List.fold_left (fun acc e -> check_entry_signature acc e true) (vars, spread_vars, new_param_var_mapping) l in
        new_vars, new_spread_vars, new_param_var_mapping in

  List.fold_left check_args_signature' (StringSet.empty, StringSet.empty, StringMap.empty) funargtyps


let check_signature ((funargtyps, rettyp) : funtyp) : unit =
  let vars, spread_vars, param_vars = check_args_signature funargtyps in

  match rettyp with
  | TypeInt -> ()
  | Nparray l ->
      ignore (List.fold_left (fun acc e -> check_entry_signature acc e false)
                             (vars, spread_vars, param_vars) l)


let rec binop_to_expr_from_mapping (s : entry) (mapping : Z3.Expr.expr StringMap.t) : Z3.Expr.expr =
  match s with
  | Id x -> StringMap.find x mapping
  | Add (s1, s2) ->
      let left = binop_to_expr_from_mapping s1 mapping in
      let right = binop_to_expr_from_mapping s2 mapping in
      Z3.Arithmetic.mk_add Z3utils.ctx [left; right]
  | Mul (s1, s2) ->
      let left = binop_to_expr_from_mapping s1 mapping in
      let right = binop_to_expr_from_mapping s2 mapping in
      Z3.Arithmetic.mk_mul Z3utils.ctx [left; right]
  | _ -> raise (TypeError "Called with wrong argument")


let rec check_app' (funargtyps : (string * typ) list)
                   (argtyps : arg list)
                   (mappings : mapping_type)
                 : mapping_type option =
  match funargtyps, argtyps with
  | [], [] -> Some mappings
  | (_, funargtyp) :: restfunargtyps, argtyp :: restargtyps ->
      check_and_update_mapping funargtyp argtyp mappings restfunargtyps restargtyps
  | _ -> failwith "impossible"


and check_and_update_mapping (curr_typ : typ)
                             (l2 : arg)
                             (mapping : mapping_type)
                             (restfunargstyps : (string * typ) list)
                             (restargtyps : arg list)
                           : mapping_type option =

    match curr_typ, l2 with
    (* the type is int and an int was provided *)
    | TypeInt, LiteralInt _
    | TypeInt, Int
    | TypeInt, Dimensions []
    | Nparray [], LiteralInt _
    | Nparray [], Int -> check_app' restfunargstyps restargtyps mapping

    (* the type is nparray and list of dimensions was provided *)
    | Nparray l1, Dimensions l2 ->

        begin match l1, l2 with
        | [], [] -> check_app' restfunargstyps restargtyps mapping
        | [], _ -> None
        | _, [] -> None
        | h :: t, args -> check_and_update_individual_mapping h args mapping restfunargstyps restargtyps t
        end

    (* no other pairing is well-typed *)
    | _, _ -> None


and check_and_update_individual_mapping (s1 : entry) (* the signature's type *)
                                        (s2 : string list) (* the remaining things in the argument *)
                                        ((var_mapping, spread_mapping, param_mapping) as mapping : mapping_type) (* the mappings thus far *)
                                        (restfunargstyps : (string * typ) list)
                                        (restargtyps : arg list)
                                        (restentries : entry list)
                                      : mapping_type option =
  match s1 with
  | Id x ->
      begin match s2 with
      | [] -> None (* no more args left, intractable *)
      | h :: t -> (* there are args left, take the first one *)
          begin match StringMap.find_opt x var_mapping with
          | None -> (* if variable is not yet mapped, store a new mapping and continue typechecking *)
              let new_var_mapping = StringMap.add x (Z3utils.mk_int h) var_mapping in
              check_and_update_mapping (Nparray restentries) (Dimensions t)
                                       (new_var_mapping, spread_mapping, param_mapping)
                                       restfunargstyps restargtyps

          | Some exp -> (* if we mapped variable already, then try to prove it's equal to what's already stored *)
              if Z3utils.prove_int_eq exp (Z3utils.mk_int h) then
              check_and_update_mapping (Nparray restentries) (Dimensions t) mapping restfunargstyps restargtyps
              else None
          end
      end

  | Add _
  | Mul _ ->
      begin match s2 with
      | [] -> None (* no more args left, intractable *)
      | h :: t -> (* there are args left, try to prove the equality *)
          let expr_e = binop_to_expr_from_mapping s1 var_mapping in
          if Z3utils.prove_int_eq expr_e (Z3utils.mk_int h) then
          check_and_update_mapping (Nparray restentries) (Dimensions t) mapping restfunargstyps restargtyps
          else None
       end

  | Spread v -> (* spread operator, capturing 0 or more variables *)
      begin match StringMap.find_opt v spread_mapping with
      | None -> (* haven't mapped this spread variable yet *)
          let split_rem = Utils.all_splits s2 in

          let rec try_splits (splits : (string list * string list) list) =
            begin match splits with
            | [] -> None
            | (front, back) :: t ->
                let new_spread_mapping = StringMap.add v front spread_mapping in
                let mapping_attempt = (* attempt to continue the typechecking with this mapping *)
                  check_and_update_mapping (Nparray restentries) (Dimensions back)
                                           (var_mapping, new_spread_mapping, param_mapping) restfunargstyps restargtyps in

                begin match mapping_attempt with
                | None -> try_splits t (* that mapping failed, try the next one instead *)

                | Some mapping -> Some mapping (* the mapping succeeded through the end! *)
                end
            end in

          try_splits split_rem

      | Some l -> (* we've already mapped the spread variable to a list of vars *)
          if List.length l > List.length s2 then None else (* there aren't enough variables left *)
          let front, back = Utils.take_n s2 (List.length l) in

          (* check if we can determine equality between the two lists *)
          if List.combine l front
            |> List.fold_left (fun acc (left, right) -> acc
                                 && Z3utils.prove_int_eq (Z3utils.mk_int left) (Z3utils.mk_int right)) true

          (* if yes, then we can continue the type checking *)
          then check_and_update_mapping (Nparray restentries) (Dimensions back) mapping restfunargstyps restargtyps

          (* if no, we have to bail out *)
          else None
      end

  | Drop (arr_name, indices)
  | Keep (arr_name, indices) ->
      let arr = StringMap.find arr_name spread_mapping in
      let concrete_indices = List.map (fun index ->
        match StringMap.find index param_mapping with
        | LiteralInt i -> i
        | _ -> raise (TypeError "Literal integer needed as argument to drop")) indices in

      let func =
        match s1 with
        | Drop _ -> Utils.drop
        | Keep _ -> Utils.keep
        | _ -> failwith "panic" in

      begin match func arr concrete_indices with
      | None -> None
      | Some reduced_arr ->
          if List.length reduced_arr > List.length s2 then None else
          let front, back = Utils.take_n s2 (List.length reduced_arr) in

          if List.combine front reduced_arr
            |> List.fold_left (fun acc (left, right) -> acc
                                 && Z3utils.prove_int_eq (Z3utils.mk_int left) (Z3utils.mk_int right)) true
          then check_and_update_mapping (Nparray restentries) (Dimensions back) mapping restfunargstyps restargtyps

          else None

      end


let check_ret_type_with_mapping (rettyp : typ) ((var_mapping, spread_mapping, param_mapping) : mapping_type) : arg =
  match rettyp with
  | TypeInt -> Int
  | Nparray l ->

      let rec check_ret_type_with_mapping' (l : entry list) : string list =
        match l with
        | [] -> []
        | h :: t ->
            begin match h with
            | Id x ->
                let bound_name = StringMap.find x var_mapping |> Z3utils.add_to_solver in
                bound_name :: check_ret_type_with_mapping' t

            | Add _ | Mul _ ->
                let bound_name = binop_to_expr_from_mapping h var_mapping |> Z3utils.add_to_solver in
                bound_name :: check_ret_type_with_mapping' t

            | Spread v ->
                let args = StringMap.find v spread_mapping in
                args @ (check_ret_type_with_mapping' t)

            | Drop (arr_name, indices)
            | Keep (arr_name, indices) ->
                let arr = StringMap.find arr_name spread_mapping in

                let concrete_indices = List.map (fun index ->
                  match StringMap.find index param_mapping with
                  | LiteralInt i -> i
                  | _ -> raise (TypeError "Literal integer needed as argument to drop")) indices in

                let func =
                  match h with
                  | Drop _ -> Utils.drop
                  | Keep _ -> Utils.keep
                  | _ -> failwith "panic" in

                let new_arr = func arr concrete_indices in

                match new_arr with
                | None -> raise (TypeError "Provided indices exceeded length of variable")
                | Some res -> res @ (check_ret_type_with_mapping' t)

            end
      in

  Dimensions (check_ret_type_with_mapping' l)


let check_app ((funargtyps, rettyp) : funtyp) (argtyps : arg list) : arg =
  check_signature (funargtyps, rettyp) ;
  if List.length funargtyps <> List.length argtyps
  then raise (TypeError "Incorrect number of arguments to function") else

  let param_names = List.map fst funargtyps in

  let param_arg_pairings = List.combine param_names argtyps in

  let param_mapping =
    List.fold_left (fun map (param_name, arg) -> StringMap.add param_name arg map) StringMap.empty param_arg_pairings in

  let final_mapping = check_app' funargtyps argtyps (StringMap.empty, StringMap.empty, param_mapping) in
  match final_mapping with
  | None -> raise (TypeError "Could not type check")
  | Some mapping -> check_ret_type_with_mapping rettyp mapping
