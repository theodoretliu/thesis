(* gets all prefixes of a list including the empty prefix *)
let rec all_prefixes (l : 'a list) : 'a list list =
  match l with
  | [] -> [[]]
  | h :: t -> [] :: List.map (fun tmp -> h :: tmp) (all_prefixes t)


(* gets all suffixes of a list *)
let rec all_suffixes (l : 'a list) : 'a list list =
  match l with
  | [] -> [[]]
  | _h :: t -> l :: all_suffixes t


(* gets all splits of a list *)
let all_splits (l : 'a list) : ('a list * 'a list) list =
  List.combine (all_prefixes l) (all_suffixes l)


(* gets the n first elements of a list and return the remaining *)
let rec take_n (l : 'a list) (n : int) : ('a list * 'a list) =
  if n = 0 then ([], l) else
  match l with
  | [] -> failwith "Not enough elements"
  | h :: t ->
     let front, back = take_n t (n - 1) in
     h :: front, back


(* removes indices from a list *)
let drop (l : 'a list) (indices : int list) : 'a list option =
  let sorted_indices = List.sort compare indices in
  let rec drop' (l : 'a list) (indices : int list) i =
    match l, indices with
    | [], [] -> Some []
    | [], _ -> None
    | h :: t, [] -> Some l
    | h :: t, index :: rest ->
        if index = i then drop' t rest (i + 1)
        else match drop' t indices (i + 1) with
        | None -> None
        | Some res -> Some (h :: res)
  in

  drop' l sorted_indices 0


(* keep indices in a list *)
let keep (l : 'a list) (indices : int list) : 'a list option =
  let sorted_indices = List.sort compare indices in
  let rec keep' (l : 'a list) (indices : int list) i =
    match l, indices with
    | [], [] -> Some []
    | [], _ -> None
    | h :: t, [] -> Some []
    | h :: t, index :: rest ->
        if index <> i then keep' t indices (i + 1)
        else match keep' t rest (i + 1) with
        | None -> None
        | Some res -> Some (h :: res)
  in

  keep' l sorted_indices 0
