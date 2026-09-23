# Closing the type-system gaps in the shape checker

This branch patches the existing OCaml checker (`checker/src/typing.ml`) one gap at a time. Each step
adds tests to `checker/test/test_gaps.ml` and has a progress note here.

- [00-evaluation.md](00-evaluation.md): evaluation of `main` and the recommendation to solidify first.

| Step | Gap | Status | Note |
|---|---|---|---|
| 0 | Solidify: bugs on `main` | done | [01-solidify.md](01-solidify.md) |
| 1 | Type-level arithmetic (`-`, `//`) | done | [02-arithmetic.md](02-arithmetic.md) |
| 2 | Values in types (`SymInt`, `IntExpr`, computed axes) | done | [03-values-in-types.md](03-values-in-types.md) |

Run everything with:

```sh
cd checker
eval $(opam env --switch=. --set-switch)   # only if the opam shell hook isn't active
dune test --force
```
