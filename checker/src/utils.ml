let rec all_prefixes (l : 'a list) : 'a list list =
  match l with
  | [] -> [[]]
  | h :: t -> [] :: List.map (fun tmp -> h :: tmp) (all_prefixes t)

let rec all_suffixes (l : 'a list) : 'a list list =
  match l with
  | [] -> [[]]
  | _h :: t -> l :: all_suffixes t

let all_splits (l : 'a list) : ('a list * 'a list) list =
  List.combine (all_prefixes l) (all_suffixes l)


let rec take_n (l : 'a list) (n : int) : ('a list * 'a list) =
  if n = 0 then ([], l) else
  match l with
  | [] -> failwith "Not enough elements"
  | h :: t ->
     let front, back = take_n t (n - 1) in
     h :: front, back
