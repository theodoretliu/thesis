(* gets all prefixes of a list including the empty prefix *)
let rec all_prefixes (l : 'a list) : 'a list list =
  match l with
  | [] -> [ [] ]
  | h :: t -> [] :: List.map (fun tmp -> h :: tmp) (all_prefixes t)

(* gets all suffixes of a list *)
let rec all_suffixes (l : 'a list) : 'a list list =
  match l with [] -> [ [] ] | _h :: t -> l :: all_suffixes t

(* gets all splits of a list *)
let all_splits (l : 'a list) : ('a list * 'a list) list =
  List.combine (all_prefixes l) (all_suffixes l)

(* gets the n first elements of a list and return the remaining *)
let rec take_n (l : 'a list) (n : int) : 'a list * 'a list =
  if n = 0 then ([], l)
  else
    match l with
    | [] -> failwith "Not enough elements"
    | h :: t ->
        let front, back = take_n t (n - 1) in
        (h :: front, back)

(* takes a list and removes duplicate elements (and sorts it) *)
let make_unique (l : 'a list) : 'a list =
  let rec make_unique' (l : 'a list) (prev : 'a) : 'a list =
    match l with
    | [] -> []
    | h :: t -> if h = prev then make_unique' t prev else h :: make_unique' t h
  in

  let l = List.sort compare l in

  match l with [] -> [] | h :: t -> h :: make_unique' t h

(* removes indices from a list *)
let drop (l : 'a list) (indices : int list) : 'a list option =
  let len = List.length l in
  let no_neg = List.map (fun i -> if i >= 0 then i else len + i) indices in
  if List.exists (fun i -> i < 0) no_neg then None
  else
    let sorted_indices = make_unique no_neg in
    let rec drop' (l : 'a list) (indices : int list) i =
      match (l, indices) with
      | [], [] -> Some []
      | [], _ -> None
      | h :: t, [] -> Some l
      | h :: t, index :: rest -> (
          if index = i then drop' t rest (i + 1)
          else
            match drop' t indices (i + 1) with
            | None -> None
            | Some res -> Some (h :: res))
    in

    drop' l sorted_indices 0

(* keep indices in a list *)
let keep (l : 'a list) (indices : int list) : 'a list option =
  let len = List.length l in
  let no_neg = List.map (fun i -> if i >= 0 then i else len + i) indices in
  if List.exists (fun i -> i < 0) no_neg then None
  else
    let sorted_indices = make_unique no_neg in

    let rec keep' (l : 'a list) (indices : int list) i =
      match (l, indices) with
      | [], [] -> Some []
      | [], _ -> None
      | h :: t, [] -> Some []
      | h :: t, index :: rest -> (
          if index <> i then keep' t indices (i + 1)
          else
            match keep' t rest (i + 1) with
            | None -> None
            | Some res -> Some (h :: res))
    in

    keep' l sorted_indices 0

(* python-style index normalization for a list of length len *)
let normalize_index (len : int) (i : int) : int = if i >= 0 then i else len + i

(* result.(i) = l.(perm.(i)); perm must be a permutation of the indices *)
let permute (l : 'a list) (perm : int list) : 'a list option =
  let len = List.length l in
  let perm = List.map (normalize_index len) perm in
  if List.sort compare perm <> List.init len Fun.id then None
  else Some (List.map (List.nth l) perm)

(* exchange the elements at two resolved positions *)
let swap (l : 'a list) (positions : int list) : 'a list option =
  match positions with
  | [ p; q ] when p >= 0 && q >= 0 && p < List.length l && q < List.length l ->
      let a = List.nth l p and b = List.nth l q in
      Some
        (List.mapi (fun i x -> if i = p then b else if i = q then a else x) l)
  | _ -> None

(* replace the elements at indices with x *)
let set_at (l : 'a list) (indices : int list) (x : 'a) : 'a list option =
  let len = List.length l in
  let indices = List.map (normalize_index len) indices in
  if List.exists (fun i -> i < 0 || i >= len) indices then None
  else Some (List.mapi (fun i y -> if List.mem i indices then x else y) l)

(* insert x so that it ends up at index (numpy expand_dims semantics) *)
let insert_at (l : 'a list) (index : int) (x : 'a) : 'a list option =
  let len = List.length l in
  let i = normalize_index (len + 1) index in
  if i < 0 || i > len then None
  else
    let front, back = take_n l i in
    Some (front @ (x :: back))
