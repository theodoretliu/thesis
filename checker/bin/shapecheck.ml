(* shapecheck [FILE]: check a program in the JSON IR (from FILE, or stdin) and
   print one result per function as JSON:
     {"results": [{"name": "f", "error": null | "message"}, ...]}
   the exit code is 0 when everything checks, 1 when something doesn't, and 2
   when the input isn't valid IR *)

open Numpy_checking.Typing

let check_all ({ env; items } : Ir_json.program) : (string * string option) list
    =
  let error_of f =
    match f () with
    | () -> None
    | exception (TypeError m | KindError m) -> Some m
  in
  let sigs = function Sig sg -> [ sg ] | Overloads sgs -> sgs in
  (* library signatures are only kind-checked at a call, so check them all *)
  let stub_results =
    List.filter_map
      (fun (name, c) ->
        Option.map
          (fun m -> (name, Some ("in stub " ^ name ^ ": " ^ m)))
          (error_of (fun () -> List.iter check_signature (sigs c))))
      env
  in
  (* every function sees every signature, as Python resolves calls when they
     run: callers depend only on signatures, so order doesn't matter *)
  let env =
    List.map (fun ({ fd; _ } : Ir_json.item) -> (fd.name, Sig fd.sg)) items
    @ env
  in
  let results =
    List.map
      (fun ({ fd; checked } : Ir_json.item) ->
        let in_fn f () =
          try f ()
          with KindError m -> raise (KindError ("in " ^ fd.name ^ ": " ^ m))
        in
        ( fd.name,
          error_of
            (if checked then fun () -> check_fundef env fd
             else in_fn (fun () -> check_signature fd.sg)) ))
      items
  in
  stub_results @ results

let () =
  let input =
    match Sys.argv with
    | [| _ |] | [| _; "-" |] -> In_channel.input_all stdin
    | [| _; file |] -> In_channel.with_open_bin file In_channel.input_all
    | _ ->
        prerr_endline "usage: shapecheck [FILE]";
        exit 2
  in
  match Ir_json.program (Yojson.Safe.from_string input) with
  | exception (Ir_json.Bad_ir m | Yojson.Json_error m) ->
      prerr_endline ("shapecheck: invalid IR: " ^ m);
      exit 2
  | program ->
      let results = check_all program in
      let json =
        `Assoc
          [
            ( "results",
              `List
                (List.map
                   (fun (name, error) ->
                     `Assoc
                       [
                         ("name", `String name);
                         ( "error",
                           match error with
                           | Some m -> `String m
                           | None -> `Null );
                       ])
                   results) );
          ]
      in
      print_endline (Yojson.Safe.pretty_to_string json);
      exit (if List.exists (fun (_, e) -> e <> None) results then 1 else 0)
