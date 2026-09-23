# Closing the type-system gaps in the shape checker

This branch patches the existing OCaml checker (`checker/src/typing.ml`) one gap at a time. Each step
adds tests to `checker/test/test_gaps.ml` and has a progress note here.

- [00-evaluation.md](00-evaluation.md): evaluation of `main` and the recommendation to solidify first.

| Step | Gap | Status | Note |
|---|---|---|---|
| 0 | Solidify: bugs on `main` | done | [01-solidify.md](01-solidify.md) |
| 1 | Type-level arithmetic (`-`, `//`) | done | [02-arithmetic.md](02-arithmetic.md) |
| 2 | Values in types (`SymInt`, `IntExpr`, computed axes) | done | [03-values-in-types.md](03-values-in-types.md) |
| 3 | Broadcast result shapes (`Broadcasted`) | done | [04-broadcasting.md](04-broadcasting.md) |
| 4 | List functions (`Permute`, `SetAt`, `InsertAt`), `Literal` flags, overloads | done | [05-list-functions.md](05-list-functions.md) |
| 5 | `Prod`/`Rank` (reshape, flatten), refinements, existentials | done | [06-refinements.md](06-refinements.md) |
| 6 | Readable diagnostics | done | [07-diagnostics.md](07-diagnostics.md) |

Run everything with:

```sh
cd checker
eval $(opam env --switch=. --set-switch)   # only if the opam shell hook isn't active
dune test --force
```
