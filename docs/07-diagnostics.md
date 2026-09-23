# Step 6: diagnostics

**Gap:** errors on variadic mismatches are unreadable, and the diagnostic is the whole point of a shape
checker. `main` reported every argument mismatch as `TypeError "Could not type check"`.

## Changes

- `fail` records **the first failure in the furthest parameter reached**, across all backtracking. Messages
  are thunks, forced only when reporting, because backtracking produces many failures.
  "First" means spreads are read as capturing as little as possible. That matches how people read
  `[*B, k, p]`. An earlier "fewest dims left" heuristic blamed matmul's inner mismatch on the batch spread,
  which was confusing.
- Each message names the parameter, its signature, the argument's shape, and the specific expectation.
  Dims show as numbers whenever the solver pins them down (`determined_int`).
- Readable printers: `string_of_entry` (`(h - kh) + 1`, `*A`, `Broadcasted(A, B)`), `string_of_typ`,
  `string_of_arg`, `string_of_constr`. These replace the ppx `show_*` output in errors.
- Derived-dimension and precondition failures print the spreads and values involved.
- `check_overloads` lists each overload's failure.

## Examples (from `test_gaps.ml`)

```
Could not type check: parameter K (array[o, c, kh, kw], given array of shape [8, 4, 5, 5]): expected c = 3, got 4
Returned dimension (h - kh) + 1 = -1 may be negative
Cannot compute Broadcasted(A, B) for A = [3], B = [4]
Precondition not provable: kh = 5 <= h = 4
Could not type check: parameter Y (array[*B, k, p], ...): expected k = 3, got 4
No overload matches the arguments:
  overload 1: ...
  overload 2: ...
```

## Limitations

- Symbolic dims print as their Z3 names (`var25`). A frontend should keep a map from solver vars to
  source-level names (`x.shape[0]`, `batch`) and use it in `string_of_dim`.
- Messages have no source locations yet. That's also frontend work.
