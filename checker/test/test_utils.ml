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
  let _ = assert (took = ([], [])) in

  let _ =
    try
      let _took = take_n [] 1 in
      assert false
    with _ -> assert true in

  let took = take_n [1; 2; 3] 2 in
  let _ = assert (took = ([1; 2], [3])) in

  let _ =
    try
      let _took = take_n [1; 2; 3] 4 in
      assert false
    with _ -> assert true in

  ()

(* test drop *)
let _ =
  let dropping = drop [] [] in
  let _ = assert (dropping = Some []) in

  let dropping = drop [1] [] in
  let _ = assert (dropping = Some [1]) in

  let dropping = drop [1] [0] in
  let _ = assert (dropping = Some []) in

  let dropping = drop [1] [1] in
  let _ = assert (dropping = None) in

  let dropping = drop [1; 2; 3; 4] [0; 2] in
  let _ = assert (dropping = Some [2; 4]) in

  let dropping = drop [1; 2; 3; 4] [2; 1] in
  let _ = assert (dropping = Some [1; 4]) in

  let dropping = drop [1; 2; 3; 4] [-1] in
  let _ = assert (dropping = Some [1; 2; 3]) in

  let dropping = drop [1; 2; 3; 4] [-2; -1] in
  let _ = assert (dropping = Some [1; 2]) in

  let dropping = drop [1; 2; 3; 4] [0; -2; -1] in
  let _ = assert (dropping = Some [2]) in

  let dropping = drop [] [-1] in
  let _ = assert (dropping = None) in

  ()

(* test keep *)
let _ =
  let keeping = keep [] [] in
  let _ = assert (keeping = Some []) in

  let keeping = keep [] [0] in
  let _ = assert (keeping = None) in

  let keeping = keep [1; 2] [] in
  let _ = assert (keeping = Some []) in

  let keeping = keep [1;2] [0] in
  let _ = assert (keeping = Some [1]) in

  let keeping = keep [1; 2] [0; 1] in
  let _ = assert (keeping = Some [1;2]) in

  let keeping = keep [1; 2; 3] [0; 2] in
  let _ = assert (keeping = Some [1; 3]) in

  let keeping = keep [1; 2; 3; 4] [-1] in
  let _ = assert (keeping = Some [4]) in

  let keeping = keep [1; 2; 3; 4] [-2; -1] in
  let _ = assert (keeping = Some [3; 4]) in

  let keeping = keep [1; 2; 3; 4] [0; -2; -1] in
  let _ = assert (keeping = Some [1; 3; 4]) in

  let keeping = keep [] [-1] in
  let _ = assert (keeping = None) in

  ()
