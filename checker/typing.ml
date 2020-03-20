type entry =
| Id of string
| Add of entry * entry
| Mul of entry * entry
type typ = Nparray of entry list
type funtyp = typ list * typ

let rec string_of_entry e =
  match e with
  | Id s -> s
  | Add (e1, e2) -> (string_of_entry e1) ^ " + " ^ (string_of_entry e2)
  | Mul (e1, e2) -> (string_of_entry e1) ^ " * " ^ (string_of_entry e2)

let string_of_typ (Nparray l) =
  "Nparray ["
    ^ (List.map string_of_entry l |> String.concat ", ")
    ^ "]"

let print_typ t =
  print_endline (string_of_typ t)

module StringMap = Map.Make(struct type t = string let compare = compare end)

type mapping_type = Z3.Expr.expr StringMap.t

let rec entry_to_expr_from_mapping (s : entry) (mapping : mapping_type) : Z3.Expr.expr option =
  match s with
  | Id id ->
      begin match StringMap.find_opt id mapping with
      | None -> None
      | Some x -> Some x end
  | Add (s1, s2) ->
      begin match entry_to_expr_from_mapping s1 mapping,
                  entry_to_expr_from_mapping s2 mapping with
      | Some x, Some y -> Some (Z3.Arithmetic.mk_add Z3utils.ctx [x; y])
      | _, _ -> None end
  | Mul (s1, s2) ->
      begin match entry_to_expr_from_mapping s1 mapping,
                  entry_to_expr_from_mapping s2 mapping with
      | Some x, Some y -> Some (Z3.Arithmetic.mk_mul Z3utils.ctx [x; y])
      | _, _ -> None end


let rec entry_to_expr_no_mapping (s : entry) : Z3.Expr.expr =
  match s with
  | Id id -> Z3utils.mk_int id
  | Add (s1, s2) ->
      let e1 = entry_to_expr_no_mapping s1 in
      let e2 = entry_to_expr_no_mapping s2 in
      Z3.Arithmetic.mk_add Z3utils.ctx [e1; e2]
  | Mul (s1, s2) ->
      let e1 = entry_to_expr_no_mapping s1 in
      let e2 = entry_to_expr_no_mapping s2 in
      Z3.Arithmetic.mk_mul Z3utils.ctx [e1; e2]


let check_and_update_individual_mapping (s1 : entry)
                                        (s2 : entry)
                                        (mapping : mapping_type)
                                      : mapping_type option =
  let rexp = entry_to_expr_no_mapping s2 in
  match entry_to_expr_from_mapping s1 mapping with
  | None ->
      begin match s1 with
      | Id x -> Some (StringMap.add x rexp mapping)
      | _ -> None end
  | Some e ->
      if Z3utils.prove_int_eq e rexp then Some mapping else None


let check_and_update_mapping (t1 : typ) (t2 : typ) (mapping : mapping_type)
                           : Z3.Expr.expr StringMap.t option =
  let Nparray l1 = t1 in
  let Nparray l2 = t2 in
  if List.length l1 <> List.length l2 then None else
  let rec check_and_update_mapping' (l1 : entry list) (l2 : entry list)
                                    (mapping : Z3.Expr.expr StringMap.t)
                                  : Z3.Expr.expr StringMap.t option =
    match l1, l2 with
    | [], [] -> Some mapping
    | h1 :: t1, h2 :: t2 ->
        begin match check_and_update_individual_mapping h1 h2 mapping with
        | None -> None
        | Some new_mapping -> check_and_update_mapping' t1 t2 new_mapping end
    | _ -> failwith "impossible" in
  check_and_update_mapping' l1 l2 mapping

let check_ret_type_with_mapping (rettyp : typ) (mapping : mapping_type) : entry list option =
  let Nparray l = rettyp in
  let rec check_ret_type_with_mapping' l =
    match l with
    | [] -> Some []
    | h :: t ->
        begin match entry_to_expr_from_mapping h mapping with
        | None -> None
        | Some v ->
            let bound_name = Z3utils.add_to_solver v in
            begin match check_ret_type_with_mapping' t with
            | None -> None
            | Some l' -> Some (Id bound_name :: l') end end in
  check_ret_type_with_mapping' l

let check_app ((funargtyps, rettyp) : funtyp) (argtyps : typ list) : typ option =
  if List.length funargtyps <> List.length argtyps then None else
  let rec check_app' (funargtyps : typ list)
                     (argtyps : typ list)
                     (mappings : mapping_type)
                   : Z3.Expr.expr StringMap.t option =
    match funargtyps, argtyps with
    | [], [] -> Some mappings
    | funargtyp :: restfunargtyps, argtyp :: restargtyps ->
        begin match check_and_update_mapping funargtyp argtyp mappings with
        | None -> None
        | Some new_mapping ->
            check_app' restfunargtyps restargtyps new_mapping end
    | _ -> failwith "impossible" in
  match check_app' funargtyps argtyps StringMap.empty with
  | None -> None
  | Some mapping ->
      match check_ret_type_with_mapping rettyp mapping with
      | None -> None
      | Some res -> Some (Nparray res)
