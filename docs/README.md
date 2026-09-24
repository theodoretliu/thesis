# Closing the type-system gaps in the shape checker

This branch patches the existing OCaml checker (`checker/src/typing.ml`) one gap at a time. Each step
adds tests to `checker/test/test_gaps.ml` and has a progress note here.

- [00-evaluation.md](00-evaluation.md): evaluation of `main` and the recommendation to solidify first.
- [09-future-work.md](09-future-work.md): remaining work, including checking variadic function bodies.

## A jaxtyping frontend that hands off to the OCaml core

Step 9 connects the checker to real Python. The frontend (`frontend/`, see its
[README](../frontend/README.md)) reads ordinary jaxtyping-annotated files (`Float[Tensor, "*batch d"]`),
translates the supported subset to the checker's IR as JSON, and runs the OCaml CLI (`checker/bin`) on it.

```sh
(cd checker && dune build)
cd frontend && python -m shapecheck ../examples/pass/*.py
```

End-to-end tests are `examples/pass/*.py` and `examples/fail/*.py`. Each fail file has `# expect-error:`
lines. Remaining frontend work is in [09-future-work.md § Frontend](09-future-work.md#frontend).

**The next goal** is to check the Transformer from "Attention Is All You Need",
[`examples/goal/transformer.py`](../examples/goal/transformer.py). The gaps, the decisions they need, and an
order of attack are in [12-transformer-goal.md](12-transformer-goal.md). `frontend/tests/test_goal.py` is the
failing spec.

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
| 8 | Checking whole function bodies, rank-guided unfolding | done | [10-function-bodies.md](10-function-bodies.md) |
| 9 | A jaxtyping frontend (Python → JSON IR → OCaml CLI) | done | [11-frontend.md](11-frontend.md) |
| goal | Check the Transformer ("Attention Is All You Need") | 3 of 24 | [12-transformer-goal.md](12-transformer-goal.md) |
| goal 1 | Free functions: int params as dims, inferred preconditions, `-1`, tuples, `x.size(i)` | done | [13-free-functions.md](13-free-functions.md) |

## Scorecard after steps 0–9

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
| Function bodies | not checked | checked once for every caller (rigid variadics, `requires`/`ensures`) |
| Python input | none | jaxtyping-annotated files, torch stubs, `file:line` diagnostics |

Step 8 adds the body checker (`check_fundef`) and closes the smaller variadic gaps (unfolding, the
empty-list axiom, 1s broadcasting against list variables).

Not attempted: general list unification, control flow in bodies, dtypes, and classes (`nn.Module`). [09-future-work.md](09-future-work.md) lists all remaining work in order,
including the design for checking the bodies of variadic functions.

Run everything with:

```sh
cd checker
eval $(opam env --switch=. --set-switch)   # only if the opam shell hook isn't active
dune test --force
cd ../frontend
python -m unittest discover -s tests -t .
```
