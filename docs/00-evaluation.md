# Evaluation of `main` (746ff23)

## What exists

| Piece | State |
|---|---|
| `checker/` (OCaml, ~580 LoC + ~800 LoC tests) | Z3-backed checker for **one function application**: `check_app : funtyp -> arg list -> arg`. Builds and passes its tests on OCaml 5.5 / dune 3.24 in the local switch. |
| `writeup/thesis.tex` | Formal syntax and semantics (Γ/Σ/Π rules) that the checker implements, plus an evaluation over numpy ops. |
| `demo.py`, `demo/` | Early pysmt prototype and surface-syntax sketches. Not wired to the checker. |
| `spec/*.hs` | Liquid Haskell experiments, superseded by the checker. |

There is no frontend. Nothing parses Python or a shape DSL, so the checker is only exercised through
OCaml unit tests that build `funtyp` values by hand.

## Shape language on `main`

```
entry ::= Id x | Int n | Add(e, e) | Mul(e, e)       -- scalar dims
        | Spread A                                   -- variadic, any position, any number per signature
        | Drop(A, idx*) | Keep(A, idx*)              -- literal or Literal-param indices, negatives ok
        | Broadcast A                                -- argument position only
typ   ::= Nparray entry* | TypeInt
arg   ::= Dimensions [z3 var]* | LiteralInt n | Int
```

Matching is backtracking over all splits for `Spread`. Equalities are **proved** (`¬φ` unsat) against a
global Z3 solver.

## Scored against the type-system gap list

| Gap (Python typing) | `main` | Notes |
|---|---|---|
| Type-level arithmetic | **partial** | `+`, `*` only. No `-` or `//`, so conv/pool can't be written. `Int` inside `Add` crashes (bug). |
| Type-level functions | **partial** | `Drop`, `Keep` exist. No permute, insert, set-at (`keepdim`), prod, or rank. |
| One variadic per param list | **solved** | `[*A, d, *B]` and multiple spreads per signature already work, because splits backtrack. |
| Two variadics unify under broadcasting | **partial** | `Broadcast A` checks compatibility in argument position. There is no result shape, so it raises "Broadcast in return type". |
| Values in types | **missing** | `Int` args carry no identity, and `x.shape[0]` can't flow into a later `zeros(n)`. The thesis spec's `zeros(k) -> Ndarray[k]` isn't implemented (KindError). No refinements, no existentials. |
| Axis/flag args are runtime values | **partial** | Literal axes work through `Drop`/`Keep`. A computed-but-determined axis (`3 - 2`) is rejected. No `keepdim`. |
| Literal widening | n/a | No Python frontend yet. `LiteralInt` vs `Int` is explicit in the calculus. |
| `Any` leaks | **missing** | No unknown-shape or symbolic-variadic arguments. The thesis lists this as its biggest limitation. |
| Unreadable diagnostics | **bad** | Every mismatch reports the single string `"Could not type check"`. |

## Bugs found (confirmed with probe tests)

1. **`Int` inside arithmetic crashes.** `binop_to_expr_from_mapping` has no `Int` case, so `Ndarray[a + 1]` raises `TypeError "Called with wrong argument"`.
2. **Spec/impl mismatch on parameter references.** The thesis rules `CheckDimIdFoundInParams` and `TransformIdParams` let `Id k` refer to an int parameter. The kind checker rejects it, and neither matching nor return construction looks at Π.
3. **Dimensions aren't constrained to be ≥ 0.** Z3 treats them as arbitrary integers, so facts like `a + b = 0 ⇒ a = 0` aren't provable.
4. **Kind hole.** `Drop`/`Keep`/`Broadcast` nested under `Add`/`Mul` pass the kind check and fail later at runtime.

## Recommendation

Solidify the checker before building any frontend or new tooling. Its core idea is sound: symbolic dims, a
backtracking list matcher, and SMT for arithmetic. That is the same architecture the "custom shape checker"
direction proposes. The weak points are local to `typing.ml`, and each can be closed with a test-first patch.
The order is:

1. Fix the four bugs above (step 0).
2. Close the calculus gaps one at a time, each with tests and a progress note (see [README](README.md)).
3. Only then add a frontend (jaxtyping string parser + Python AST walk) on top of a checker that is
   trustworthy.
