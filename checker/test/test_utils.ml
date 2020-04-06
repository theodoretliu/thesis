open Numpy_checking.Utils

(* test all_prefixes empty *)
let _ =
  let prefixes = all_prefixes [] in
  assert (prefixes = [[]])

(* test all_prefixes of single *)
let _ =
  let prefixes = all_prefixes [1] in
  assert (prefixes = [[]; [1]])

(* test all_prefixes of multiple *)
let _ =
  let prefixes = all_prefixes ["A"; "B"; "C"] in
  assert (prefixes = [[]; ["A"]; ["A"; "B"]; ["A"; "B"; "C"]])

(* test suffixes *)
let _ =
  let suffixes = all_suffixes [] in
  assert (suffixes = [[]])

let _ =
  let suffixes = all_suffixes [1] in
  assert (suffixes = [[1]; []])

let _ =
  let suffixes = all_suffixes [1; 2; 3] in
  assert (suffixes = [[1; 2; 3]; [2; 3]; [3]; []])

(* test all_splits *)
let _ =
  let splits = all_splits [] in
  assert (splits = [[], []])

let _ =
  let splits = all_splits [1] in
  assert (splits = [[], [1]; [1], []])

let _ =
  let splits = all_splits ["A"; "B"; "C"] in
  assert (splits = [[], ["A"; "B"; "C"]; ["A"], ["B"; "C"];
                    ["A"; "B"], ["C"]; ["A"; "B"; "C"], []])

(* test take_n *)
let _ =
  let took = take_n [] 0 in
  assert (took = ([], []))

let _ =
  try
    let _took = take_n [] 1 in
    assert false
  with _ -> assert true

let _ =
  let took = take_n [1; 2; 3] 2 in
  assert (took = ([1; 2], [3]))

let _ =
  try
    let _took = take_n [1; 2; 3] 4 in
    assert false
  with _ -> assert true
