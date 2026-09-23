# Step 2: values in types

**Gap:** `x.shape[0]` is an `int` and can never become a dimension. Axis arguments must be literals.
In the thesis, `sum(y, 3 - 2)` fails because the axis isn't a `LiteralInt`.

## Changes

- **`SymInt v` argument.** An int whose value is the Z3 variable `v`. A frontend gives
  `n = x.shape[0]` the type `SymInt` of `x`'s first dim var, so `n` keeps its identity across calls.
  `Int` stays as the opaque "some int" argument.
- **`IntExpr e` type.**
  - *Return position:* the result is a `SymInt` equal to `e`. For example, `shape0(x: [n, *R]) -> int{n}`,
    `size(x: [a, b]) -> int{a*b}`, `add(i, j) -> int{i + j}`.
  - *Parameter position:* the int argument must provably equal `e`, e.g. `at(x: [n], i: int{n - 1})`.
    `e` may only mention dims bound by earlier parameters.
- **Determined indices.** `Drop`/`Keep` indices from a `SymInt` are accepted when the solver's constraints
  force a unique value (`Z3utils.determined_int`: take a model value, then prove it's the only one).
- `Id k` for an int parameter now works with `SymInt` too. A returned `[k]` then needs `k ≥ 0` to be
  provable, so `zeros` of an unconstrained symbolic int is rejected. An opaque `Int` still yields a fresh
  dimension (lenient, as in step 0).

## Now expressible

```python
n = x.shape[0]          # shape0: SymInt n
w = zeros(n)            # Ndarray[n]
scale(x, w)             # ok: proves n = n
scale(x, zeros(n + 1))  # rejected
sum(x, 3 - 2)           # ok: axis is determined to be 1
```

## Not covered

- Refinements (`n > 0`) and existentials (`nonzero`): next steps.
- Getting a `SymInt` from `x.shape[i]` for a *symbolic* `i` needs an index-into-spread dimension. A frontend
  can read `x.shape[i]` straight off the argument's dim list when `i` is a literal, so this is deferred.
