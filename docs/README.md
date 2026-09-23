# Closing the type-system gaps in the shape checker

This branch patches the existing OCaml checker (`checker/src/typing.ml`) one gap at a time. Each step
adds tests to `checker/test/test_gaps.ml` and has a progress note here.

- [00-evaluation.md](00-evaluation.md): evaluation of `main` and the recommendation to solidify first.
- [09-future-work.md](09-future-work.md): remaining work, including checking variadic function bodies.

| Step | Gap | Status | Note |
|---|---|---|---|
| 0 | Solidify: bugs on `main` | done | [01-solidify.md](01-solidify.md) |
| 1 | Type-level arithmetic (`-`, `//`) | done | [02-arithmetic.md](02-arithmetic.md) |
| 2 | Values in types (`SymInt`, `IntExpr`, computed axes) | done | [03-values-in-types.md](03-values-in-types.md) |
| 3 | Broadcast result shapes (`Broadcasted`) | done | [04-broadcasting.md](04-broadcasting.md) |
| 4 | List functions (`Permute`, `SetAt`, `InsertAt`), `Literal` flags, overloads | done | [05-list-functions.md](05-list-functions.md) |
| 5 | `Prod`/`Rank` (reshape, flatten), refinements, existentials | done | [06-refinements.md](06-refinements.md) |
| 6 | Readable diagnostics | done | [07-diagnostics.md](07-diagnostics.md) |
| 7 | Symbolic variadic arguments (function bodies, `Any`) | done | [08-symbolic-variadics.md](08-symbolic-variadics.md) |

## Scorecard after steps 0–7

| Gap | `main` | Now |
|---|---|---|
| Type-level arithmetic | `+ *` (and `Int` inside crashed) | `+ - * //`, `Prod`, `Rank`, well-definedness proofs |
| Type-level functions | `Drop`, `Keep` | + `Permute`, `SetAt`, `InsertAt`, `Broadcasted` |
| One variadic per param list | already solved | unchanged |
| Two variadics under broadcasting | check only | result shapes (`Broadcasted`), batched matmul |
| Values in types | none (spec'd param refs unimplemented) | `SymInt`, `IntExpr`, param refs, refinements, existentials |
| Axis/flag args | literal axes | determined symbolic axes, `Literal` flags, overloads, `keepdim` |
| `Any` leaks | args needed a known number of dims | list variables; `Any = [*fresh]` is used soundly or rejected |
| Unreadable diagnostics | "Could not type check" | parameter, signature, shape, and the specific expectation |

Not attempted: the body checker, list unification, dtypes, a frontend (jaxtyping strings + Python AST), and
source-level names in messages. [09-future-work.md](09-future-work.md) lists all remaining work in order,
including the design for checking the bodies of variadic functions.

Run everything with:

```sh
cd checker
eval $(opam env --switch=. --set-switch)   # only if the opam shell hook isn't active
dune test --force
```
