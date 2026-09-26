(* shapecheck [FILE]: check a program in the JSON IR (from FILE, or stdin) and
   print one result per function as JSON:
     {"results": [{"name": "f", "error": null | "message",
                   "inferred": ["n >= 0", ...]}, ...]}
   inferred lists the preconditions the checker added to f's signature. the
   exit code is 0 when everything checks, 1 when something doesn't, and 2 when
   the input isn't valid IR *)

open Numpy_checking.Typing

type result = { name : string; error : string option; inferred : constr list }

(* a bound inferred for a parameter, e.g. n >= 0 *)
let string_of_inferred = function
  | Le (Int k, Id x) -> x ^ " >= " ^ string_of_int k
  | c -> show_constr c

(* bodies may infer preconditions, which callers' bodies then have to prove,
   and may infer in turn. rounds repeat until nothing new is inferred; each
   adds a sign fact about a parameter, so there are few *)
let max_rounds = 20

(* a constructor's requires, over a method's parameters: ints pairs the
   constructor's parameters with the method's. a fact about anything else
   (a spread, or a parameter that isn't an instance int) is dropped *)
let rename_constr (ints : (string * string) list) (c : constr) : constr option =
  let both mk f a b =
    match (f a, f b) with Some a, Some b -> Some (mk a b) | _ -> None
  in
  let rec entry = function
    | Id x -> Option.map (fun y -> Id y) (List.assoc_opt x ints)
    | Int i -> Some (Int i)
    | Add (a, b) -> both (fun a b -> Add (a, b)) entry a b
    | Sub (a, b) -> both (fun a b -> Sub (a, b)) entry a b
    | Mul (a, b) -> both (fun a b -> Mul (a, b)) entry a b
    | Div (a, b) -> both (fun a b -> Div (a, b)) entry a b
    | _ -> None
  in
  match c with
  | Eq (a, b) -> both (fun a b -> Eq (a, b)) entry a b
  | Le (a, b) -> both (fun a b -> Le (a, b)) entry a b
  | Lt (a, b) -> both (fun a b -> Lt (a, b)) entry a b

let check_all ({ env; items } : Ir_json.program) : result list =
  let error_of f =
    match f () with
    | x -> Ok x
    | exception (TypeError m | KindError m) -> Error m
  in
  let sigs = function Sig sg -> [ sg ] | Overloads sgs -> sgs in
  (* library signatures are only kind-checked at a call, so check them all *)
  let stub_results =
    List.filter_map
      (fun ({ name; callee; _ } : Ir_json.lib) ->
        match error_of (fun () -> List.iter check_signature (sigs callee)) with
        | Ok () -> None
        | Error m ->
            Some
              {
                name;
                error = Some ("in stub " ^ name ^ ": " ^ m);
                inferred = [];
              })
      env
  in
  let inferred_for inferred name =
    Option.value ~default:[] (List.assoc_opt name inferred)
  in
  let with_requires inferred (fd : fundef) =
    {
      fd with
      sg =
        { fd.sg with requires = fd.sg.requires @ inferred_for inferred fd.name };
    }
  in
  (* what a constructor guarantees about its ints: its requires, including
     those inferred so far, then its ensures (asserts in __init__), which may
     need the requires to be stated *)
  let guarantees_of inferred name =
    match
      List.find_opt (fun ({ fd; _ } : Ir_json.item) -> fd.name = name) items
    with
    | Some { fd; _ } ->
        fd.sg.requires @ inferred_for inferred name @ fd.sg.ensures
    | None -> (
        match
          List.find_opt (fun ({ name = n; _ } : Ir_json.lib) -> n = name) env
        with
        | Some { callee = Sig sg; _ } -> sg.requires @ sg.ensures
        | _ -> [])
  in
  (* every instance was built by its constructor, so it satisfies what the
     constructor guarantees *)
  let with_invariant inferred instances (sg : signature) =
    let facts =
      List.concat_map
        (fun ({ init; ints } : Ir_json.instance) ->
          List.filter_map (rename_constr ints) (guarantees_of inferred init))
        instances
    in
    { sg with invariant = sg.invariant @ facts }
  in
  let lib_callee inferred ({ name; callee; lib_instances } : Ir_json.lib) =
    let resolve = with_invariant inferred lib_instances in
    ( name,
      match callee with
      | Sig sg -> Sig (resolve sg)
      | Overloads sgs -> Overloads (List.map resolve sgs) )
  in
  (* every function sees every signature, as Python resolves calls when they
     run: callers depend only on signatures, so order doesn't matter. only
     the functions only selects are checked *)
  let check_round ~infer ~only inferred =
    let fds =
      List.map
        (fun ({ fd; checked; instances } : Ir_json.item) ->
          let fd = with_requires inferred fd in
          ({ fd with sg = with_invariant inferred instances fd.sg }, checked))
        items
    in
    let env =
      List.map (fun ((fd : fundef), _) -> (fd.name, Sig fd.sg)) fds
      @ List.map (lib_callee inferred) env
    in
    List.filter_map
      (fun ((fd : fundef), checked) ->
        let in_fn f () =
          try f ()
          with KindError m -> raise (KindError ("in " ^ fd.name ^ ": " ^ m))
        in
        if not (only fd.name) then None
        else
          Some
            ( fd.name,
              error_of
                (if not checked then
                   in_fn (fun () ->
                       check_signature fd.sg;
                       [])
                 else if infer then fun () -> infer_requires env fd
                 else fun () ->
                   check_fundef env fd;
                   []) ))
      fds
  in
  let rec rounds ~only inferred n =
    let results = check_round ~infer:true ~only inferred in
    let fresh =
      List.filter_map
        (function name, Ok (_ :: _ as cs) -> Some (name, cs) | _ -> None)
        results
    in
    if fresh = [] then (results, inferred)
    else
      let inferred =
        List.map
          (fun (name, cs) -> (name, inferred_for inferred name @ cs))
          fresh
        @ List.filter
            (fun (name, _) -> not (List.mem_assoc name fresh))
            inferred
      in
      (* the last round's bodies assumed facts their callers didn't see *)
      if n >= max_rounds then (check_round ~infer:false ~only inferred, inferred)
      else rounds ~only inferred (n + 1)
  in
  (* constructors first: what they infer is their instances' invariant, so a
     method can assume it instead of inferring it as a requires of its own *)
  let inits =
    List.concat_map
      (fun ({ instances; _ } : Ir_json.item) ->
        List.map (fun ({ init; _ } : Ir_json.instance) -> init) instances)
      items
  in
  let _, seed = rounds ~only:(fun name -> List.mem name inits) [] 1 in
  let results, inferred = rounds ~only:(fun _ -> true) seed 1 in
  stub_results
  @ List.map
      (fun (name, r) ->
        {
          name;
          error = (match r with Ok _ -> None | Error m -> Some m);
          inferred = inferred_for inferred name;
        })
      results

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
                   (fun { name; error; inferred } ->
                     `Assoc
                       [
                         ("name", `String name);
                         ( "error",
                           match error with
                           | Some m -> `String m
                           | None -> `Null );
                         ( "inferred",
                           `List
                             (List.map
                                (fun c -> `String (string_of_inferred c))
                                inferred) );
                       ])
                   results) );
          ]
      in
      print_endline (Yojson.Safe.pretty_to_string json);
      exit (if List.exists (fun r -> r.error <> None) results then 1 else 0)
