# Step 0: solidify the checker

These are fixes to behavior that `main` already claimed, either in its tests or in the thesis semantics.
No new surface syntax.

## Changes

| Bug | Fix |
|---|---|
| `Int` inside `Add`/`Mul` raised `TypeError "Called with wrong argument"` | `binop_to_expr_from_mapping` is replaced by `expr_of_dim`, which handles `Id`, `Int`, `Add`, `Mul`. |
| Thesis rules `CheckDimIdFoundInParams` and `TransformIdParams` weren't implemented | `Id k` may now name an integer parameter (`TypeInt` or `Nparray []`), in argument or return position. In an argument it must be provably equal to the literal. In a return it becomes that value, or a fresh dimension if the int isn't a literal. Array parameters used as dimensions are a `KindError`. |
| Dimensions were unconstrained Z3 integers | Every argument dimension and every dimension the checker creates is asserted `≥ 0` (`Z3utils.assume_dim`, `fresh_dim`). Negative `Int` literals as dimensions are a `KindError`. |
| `Drop`/`Keep`/`Broadcast` under `Add`/`Mul` passed the kind check | New `check_arith_signature` recursively allows only bound ids, int params, and literals. |

## Example now accepted

```ocaml
(* zeros(k: int) -> Ndarray[k] *)
check_app ([ ("k", TypeInt) ], Nparray [ Id "k" ]) [ LiteralInt 3 ]   (* Ndarray[3] *)

(* take(n: int, x: Ndarray[n, d]) -> Ndarray[d] *)
check_app ([ ("n", TypeInt); ("X", Nparray [ Id "n"; Id "d" ]) ], Nparray [ Id "d" ])
  [ LiteralInt 3; Dimensions [ x3; d ] ]
```

## Test harness

`test_gaps.ml` has an `expect` helper that prints `FAIL <name>` and exits non-zero, so `dune test` goes red.
The original tests use bare `assert` and are unchanged.

## Not changed yet

- `TypeError "Could not type check"` is still the only application-level diagnostic (step 7).
- The Z3 solver is still one global, assertion-accumulating instance. That's fine for a single checking
  run, but it needs push/pop scoping once there's a frontend that checks many functions.
