type entry = string
type typ = Nparray of entry list
type funtyp = typ list * typ

let string_of_typ (Nparray l) =
  "Nparray [" ^ (String.concat ", " l) ^ "]"

let print_typ t =
  print_string (string_of_typ t)

module StringMap = Map.Make(struct type t = string let compare = compare end)

let prove_equal v1 v2 = v1 = v2

let check_and_update_individual_mapping s1 s2 mapping =
  print_string ("s1" ^ s1) ;
  print_string ("s2" ^ s2) ;
  match StringMap.find_opt s1 mapping with
  | None -> Some (StringMap.add s1 s2 mapping)
  | Some v ->
      if prove_equal v s2 then Some (StringMap.add s1 s2 mapping)
      else None

let check_and_update_mapping t1 t2 mapping =
  let Nparray l1 = t1 in
  let Nparray l2 = t2 in
  if List.length l1 <> List.length l2 then None else
  let rec check_and_update_mapping' l1 l2 mapping =
    match l1, l2 with
    | [], [] -> Some mapping
    | h1 :: t1, h2 :: t2 ->
        begin match check_and_update_individual_mapping h1 h2 mapping with
        | None -> None
        | Some new_mapping -> check_and_update_mapping' t1 t2 new_mapping end
    | _ -> failwith "impossible" in
  check_and_update_mapping' l1 l2 mapping

let check_ret_type_with_mapping rettyp mapping =
  print_typ rettyp ;
  let _ = StringMap.mapi (fun k v -> print_string k ; print_string v ; print_newline ()) mapping in
  let Nparray l = rettyp in
  let rec check_ret_type_with_mapping' l =
    match l with
    | [] -> Some []
    | h :: t ->
        begin match StringMap.find_opt h mapping with
        | None -> None
        | Some v ->
            begin match check_ret_type_with_mapping' t with
            | None -> None
            | Some l' -> Some (v :: l') end end in
  check_ret_type_with_mapping' l

let check_app ((funargtyps, rettyp) : funtyp) (argtyps : typ list) : typ option =
  if List.length funargtyps <> List.length argtyps then None else
  let rec check_app' funargtyps argtyps mappings =
    print_endline "check_app'" ;
    match funargtyps, argtyps with
    | [], [] -> Some mappings
    | funargtyp :: restfunargtyps, argtyp :: restargtyps ->
        begin match check_and_update_mapping funargtyp argtyp mappings with
        | None -> None
        | Some new_mapping ->
            check_app' restfunargtyps restargtyps new_mapping end
    | _ -> failwith "impossible" in
  match check_app' funargtyps argtyps StringMap.empty with
  | None -> print_endline "check_app' got None" ; None
  | Some mapping ->
      match check_ret_type_with_mapping rettyp mapping with
      | None -> None
      | Some res -> Some (Nparray res)
