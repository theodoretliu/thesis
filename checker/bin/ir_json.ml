(* decoding the checker IR from the JSON the Python frontend emits; see
   frontend/README.md for the format *)

open Numpy_checking.Typing

exception Bad_ir of string

let bad what (j : Yojson.Safe.t) =
  raise (Bad_ir ("expected " ^ what ^ ", got " ^ Yojson.Safe.to_string j))

let string = function `String s -> s | j -> bad "a string" j
let int = function `Int i -> i | j -> bad "an int" j
let list f = function `List l -> List.map f l | j -> bad "a list" j

let field name = function
  | `Assoc kvs -> (
      match List.assoc_opt name kvs with
      | Some v -> v
      | None -> raise (Bad_ir ("missing field " ^ name)))
  | j -> bad "an object" j

(* a field that may be left out *)
let optional name default f = function
  | `Assoc kvs as j when List.mem_assoc name kvs -> f (field name j)
  | _ -> default

let index : Yojson.Safe.t -> (string, int) either = function
  | `String s -> Left s
  | `Int i -> Right i
  | j -> bad "an index (a name or an int)" j

let rec entry (j : Yojson.Safe.t) : entry =
  match j with
  | `List [ `String "Id"; x ] -> Id (string x)
  | `List [ `String "Int"; i ] -> Int (int i)
  | `List [ `String "Add"; a; b ] -> Add (entry a, entry b)
  | `List [ `String "Sub"; a; b ] -> Sub (entry a, entry b)
  | `List [ `String "Mul"; a; b ] -> Mul (entry a, entry b)
  | `List [ `String "Div"; a; b ] -> Div (entry a, entry b)
  | `List [ `String "Spread"; a ] -> Spread (string a)
  | `List [ `String "Broadcast"; a ] -> Broadcast (string a)
  | `List [ `String "Broadcasted"; l ] -> Broadcasted (list string l)
  | `List [ `String "Drop"; a; l ] -> Drop (string a, list index l)
  | `List [ `String "Keep"; a; l ] -> Keep (string a, list index l)
  | `List [ `String "Permute"; a; l ] -> Permute (string a, list index l)
  | `List [ `String "SetAt"; a; l; d ] -> SetAt (string a, list index l, entry d)
  | `List [ `String "InsertAt"; a; i; d ] ->
      InsertAt (string a, index i, entry d)
  | `List [ `String "Prod"; a ] -> Prod (string a)
  | `List [ `String "Rank"; a ] -> Rank (string a)
  | `List [ `String "Index"; a; i ] -> Index (string a, index i)
  | j -> bad "a dimension entry" j

let rec typ (j : Yojson.Safe.t) : typ =
  match j with
  | `List [ `String "Array"; l ] -> Nparray (list entry l)
  | `List [ `String "Int" ] -> TypeInt
  | `List [ `String "IntExpr"; e ] -> IntExpr (entry e)
  | `List [ `String "Literal"; i ] -> TypeLiteralInt (int i)
  | `List [ `String "Tuple"; l ] -> TypeTuple (list typ l)
  | j -> bad "a type" j

let constr (j : Yojson.Safe.t) : constr =
  match j with
  | `List [ `String "Eq"; a; b ] -> Eq (entry a, entry b)
  | `List [ `String "Le"; a; b ] -> Le (entry a, entry b)
  | `List [ `String "Lt"; a; b ] -> Lt (entry a, entry b)
  | j -> bad "a constraint" j

let param (j : Yojson.Safe.t) : string * typ =
  match j with
  | `List [ name; t ] -> (string name, typ t)
  | j -> bad "a parameter [name, type]" j

let signature (j : Yojson.Safe.t) : signature =
  {
    params = list param (field "params" j);
    ret = typ (field "ret" j);
    requires = list constr (field "requires" j);
    exists = list string (field "exists" j);
    ensures = list constr (field "ensures" j);
    invariant = optional "invariant" [] (list constr) j;
  }

(* a group of int parameters that are an instance's constructor ints: init
   names the constructor, and ints pairs its parameters with the signature's.
   the constructor's requires become the signature's invariant *)
type instance = { init : string; ints : (string * string) list }

let instance (j : Yojson.Safe.t) : instance =
  let pair = function
    | `List [ a; b ] -> (string a, string b)
    | j -> bad "a pair [constructor parameter, parameter]" j
  in
  { init = string (field "init" j); ints = list pair (field "ints" j) }

let instances (j : Yojson.Safe.t) : instance list =
  optional "instances" [] (list instance) j

let rec term (j : Yojson.Safe.t) : term =
  match j with
  | `List [ `String "Var"; x ] -> Var (string x)
  | `List [ `String "Lit"; i ] -> Lit (int i)
  | `List [ `String "Call"; f; args ] -> Call (string f, list term args)
  | `List [ `String "Shape"; ts ] -> Shape (list term ts)
  | `List [ `String "Scalar" ] -> Scalar
  | `List [ `String "Tuple"; ts ] -> Tup (list term ts)
  | j -> bad "a term" j

let rec stmt (j : Yojson.Safe.t) : stmt =
  match j with
  | `List [ `String "Let"; x; t ] -> Let (string x, term t)
  | `List [ `String "LetAnnot"; x; ty; t ] -> LetAnnot (string x, typ ty, term t)
  | `List [ `String "Return"; t ] -> Return (term t)
  | `List [ `String "Unpack"; xs; t ] -> Unpack (list string xs, term t)
  | `List [ `String "At"; line; text; s ] -> At (int line, string text, stmt s)
  | j -> bad "a statement" j

(* a library function: one signature, or several tried in order. its
   overloads are methods of one class, or none, so they share instances *)
type lib = { name : string; callee : callee; lib_instances : instance list }

let callee (j : Yojson.Safe.t) : lib =
  let name = string (field "name" j) in
  let overloads = match field "overloads" j with `List l -> l | j -> [ j ] in
  let lib_instances =
    match overloads with sg :: _ -> instances sg | [] -> []
  in
  match List.map signature overloads with
  | [ sg ] -> { name; callee = Sig sg; lib_instances }
  | sgs -> { name; callee = Overloads sgs; lib_instances }

(* a function to check; without a body only its signature is used *)
type item = { fd : fundef; checked : bool; instances : instance list }

let item (j : Yojson.Safe.t) : item =
  let name = string (field "name" j) and sg = signature (field "sig" j) in
  let instances = instances (field "sig" j) in
  match field "body" j with
  | `Null -> { fd = { name; sg; body = [] }; checked = false; instances }
  | body ->
      { fd = { name; sg; body = list stmt body }; checked = true; instances }

type program = { env : lib list; items : item list }

let program (j : Yojson.Safe.t) : program =
  { env = list callee (field "env" j); items = list item (field "functions" j) }
