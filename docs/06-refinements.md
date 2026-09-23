# Step 5: reshape/flatten, refinements, existentials

**Gaps:**
- No `prod(shape)`, so reshape and flatten are unwritable.
- No refinements (`n > 0`).
- No existentials for data-dependent shapes (`nonzero`, `unique`).

## Changes

- **`Prod A`** and **`Rank A`** are arithmetic dimensions over a bound spread. The empty product is 1. They
  compose with `+ - * //`.
- **`signature`** record: `{ params; ret; requires; exists; ensures }` with
  `constr ::= Eq | Le | Lt` over arithmetic dimensions. Check it with `check_sig`.
  `check_app` is now `check_sig` with no refinements, so old callers are unchanged.
  - `requires` must be **provable** after the arguments match, otherwise
    `TypeError "Precondition not provable: …"`.
  - `exists` names become fresh natural dims, available in `ensures` and the return type.
  - `ensures` are **assumed** (asserted into the solver) about the new dims.
- The kind checker covers constraints and existentials. An existential may not shadow a parameter dim.
- `expr_of_dim` now takes the whole mapping, so arithmetic can see spreads.

## Now expressible

```ocaml
flatten : [b, *A] -> [b, Prod A]
reshape : [*A], shape:[*B] -> [*B]                       requires Prod A = Prod B
conv2d  : ... -> [n, o, h - kh + 1, w - kw + 1]           requires kh <= h, kw <= w
nonzero : [*A] -> exists k. [k, Rank A]                  ensures k <= Prod A
unique  : [n]  -> exists m. [m]                          ensures m <= n
```

The tests chain `conv2d → flatten → linear(6272 → 10)`. They check that `reshape [a, b] → [b, a]` is proved
symbolically, that the strict conv closes the step 1 limitation, and that two `unique` results aren't
assumed equal.

## Limitations

- `requires` is checked *after* matching, so it doesn't drive backtracking over spread splits. It
  only matters for signatures with ambiguous splits (`[*A, *B]`), which are rare.
- Int tuples such as `reshape`'s `shape` are passed as `Dimensions` (a list of naturals). `-1` inference
  isn't supported.
- Nonlinear constraints (`Prod` of several symbolic dims) go to Z3's nonlinear arithmetic, which may
  return *unknown*. That's treated as "not provable", which is sound but may reject valid code.
