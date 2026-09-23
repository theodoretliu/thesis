# Step 4: type-level list functions, axis and flag arguments

**Gaps:**
- Python has no type-level functions: no `DeleteAt`, `Permute`, and so on.
- Axis/flag arguments are runtime values, so `sum(x, dim=1, keepdim=True)` needs an explosion of overloads.

`main` had `Drop` and `Keep`, but no permute, keepdim, or unsqueeze, and no way to pick a signature by a flag.

## Changes

New derived-list dimensions. They all take a bound spread and literal or int-parameter indices, with Python
negative indexing:

| Constructor | Meaning | Use |
|---|---|---|
| `Permute (A, [p...])` | `result[i] = A[p[i]]`; `p` must be a permutation | `permute`, `transpose`, `movedim` |
| `SetAt (A, [i...], d)` | replace dims at `i...` with arithmetic `d` | `keepdim=True` (`d = 1`) |
| `InsertAt (A, i, d)` | insert `d` so it lands at index `i` (`expand_dims` semantics) | `unsqueeze`, `None` indexing |

An inserted or set `d` must be provably ≥ 0. Its arithmetic is kind-checked like any other dimension.
List helpers live in `Utils` (`permute`, `set_at`, `insert_at`) and have unit tests.

Flags and overloads:

- **`TypeLiteralInt i`** parameter type (`Literal[i]`, with bools as `0`/`1`). It accepts `LiteralInt i` or a
  `SymInt` provably equal to `i`. As a return type it yields `LiteralInt i`.
- **`check_overloads`** tries signatures in order and returns the first that checks, like `@overload`.
  `KindError`s propagate so broken signatures stay loud.

## Now expressible

```ocaml
(* sum(x, dim, keepdim) *)
[ ([X: [*A]; dim: int; keepdim: Literal[1]], [SetAt (A, [dim], 1)]);
  ([X: [*A]; dim: int; keepdim: Literal[0]], [Drop (A, [dim])]) ]
(* unsqueeze(x, d) *)   ([X: [*A]; d: int], [InsertAt (A, d, 1)])
(* x.permute(p0,p1,p2)*) ([X: [*A]; p0; p1; p2: int], [Permute (A, [p0; p1; p2])])
```

The explosion is gone: one signature per *flag value*, not per axis × sign × flag.

## Not covered

- List functions take a spread *name*, so they don't compose (`Permute (Drop (A, …))`). A small list-expression
  sublanguage would fix that. Each composition seen so far can be written by binding more spreads in the
  parameter pattern instead.
- A variable-length permutation (`permute(*dims)`) needs tuple arguments.
