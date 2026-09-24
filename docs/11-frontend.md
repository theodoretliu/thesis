# Step 9: a jaxtyping frontend

**Gap:** the checker could only be driven from OCaml. Signatures and bodies were built by hand in tests, so
no Python program could be checked.

## Design

The frontend follows the plan in [09-future-work.md](09-future-work.md#frontend). The IR is the contract
between two halves:

- **`frontend/shapecheck`** (Python 3.9+, stdlib only) reads jaxtyping-annotated files with `ast` and emits
  the IR as JSON. It never type-checks.
- **`checker/bin/shapecheck.exe`** decodes the JSON (`yojson`), checks every function, and reports one result
  per function. The core never sees Python.

`frontend/README.md` is the user-facing reference: annotations, the supported subset, stub syntax, and the
JSON format. The decisions behind it:

- **User code is plain jaxtyping.** Shape names and Python parameter names are separate namespaces, and
  a name only the return type mentions is existential. So users write no `requires`/`exists`/`ensures`.
  The IR shares one namespace, so a parameter that clashes with a dim is renamed (`n` becomes `n'`).
- **Stubs are the only place for refinements**, written as `assert`s in the stub body. An assert that
  mentions an existential is an `ensures`, and any other is a `requires`. Stubs also get what jaxtyping
  can't say: list functions (`*drop(A,dim)`, `*broadcast(A,B)`), `prod`/`rank`, int parameters inside
  shapes, `Dim["expr"]` for int results, and `Shape["*S"]` for int tuples and `*size` varargs.
- **The frontend binds arguments.** Keywords and defaults use Python's rules, per overload. A call goes to
  the overloads that accept its arguments, so an overload can't match arguments that Python would have
  bound to different parameters. If those overloads bind the arguments differently, the call is an error.
- **Asserts in user code are dropped.** That's sound, but it loses information: `torch.zeros(n)` for an
  `n: int` parameter is rejected because `n` may be negative. Assuming an assert after it runs is future
  work.
- **All signatures are visible everywhere.** Python resolves calls at run time, so the checker CLI adds
  every function's signature before checking any body. Definition order doesn't matter, and recursion works.
- **Errors don't stop the file.** A function the frontend can't translate still contributes its signature
  (`"body": null`), and every function is reported separately.

## Core changes

- **Terms:** `Shape of term list` turns ints into a shape (a dim equal to each int, which must be provably
  `≥ 0`). `Scalar` is a float, which is a 0-d array.
- **Located statements:** `At of int * string * stmt` carries the source line and text. Errors read
  `in f, line 7, `z = x @ w`: ...`, and the frontend turns that into `file.py:7: in f, `z = x @ w`: ...`.
- **Overloads with refinements:** `callee`'s `Overloads` now holds `signature`s, so overloads can have
  `requires` (conv2d). `check_overloads` on `funtyp`s wraps the new `check_overload_sigs`.
- **Shorter overload errors:** only the overloads that matched the most parameters are listed. The rest are
  counted, e.g. `(3 other overloads failed at an earlier parameter)`. This matters because every operator
  is overloaded on arrays and ints.
- **Readable solver terms:** `string_of_expr` prints arithmetic infix with dim labels, e.g. `h + 2` instead
  of `(+ var22 (* 2 1))`.

## Soundness notes

- In-place ops are rejected. The core's `Broadcast` only checks compatibility, so an
  `iadd(a: *A, b: *#A) -> *A` stub would accept `x += y` when `y` has extra leading dims. torch rejects
  that at run time.
- `*#a` binds `a` exactly on its first occurrence. That's stricter than jaxtyping for callers, and sound for
  the body.
- A method call on any non-module value resolves to the stub classes' method of that name. An int receiver
  then fails the check, because `self` is an array.
- A call with no stub is an error, never an unknown shape.

## Tests

- `frontend/tests/test_shapes.py` covers the shape-string parser.
- `frontend/tests/test_translate.py` covers the IR for signatures, bodies, name resolution and argument
  binding, plus every translation error.
- `frontend/tests/test_examples.py` checks every `examples/pass/*.py`, every `examples/fail/*.py` against its
  `# expect-error:` lines, and the checker CLI's errors.

The examples cover an MLP, attention and self-attention over `*batch`, a CNN (conv2d → pool → flatten →
linear), layer norm, reshape, `unique`, keyword/default arguments, and the failure modes.

## Limitations

- **Straight-line bodies only:** no `if`, loops, `with`, or `try`.
- **No `x.shape[i]`, indexing, tuples (except as shapes), lists (`torch.cat`), or `None` arguments.**
- **Top-level functions only.** `nn.Module.forward` needs classes.
- **Single broadcastable dims (`#b`) and `?` dims** aren't supported. `*#b` is.
- **Existentials from callees** print as solver names (`var2`).
