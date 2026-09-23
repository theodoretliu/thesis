# Step 1: type-level arithmetic (`-`, `//`)

**Gap:** Python types have no `Add`/`Mul`/`Sub` at the type level, so conv, pooling, and padding shapes
can't be written. `main` had `+` and `*` only.

## Changes

- New dimension constructors `Sub (e1, e2)` and `Div (e1, e2)`. `Div` is floor division, which is Z3 integer
  `div` with a positive divisor.
- `expr_of_dim` now returns `(expr, dim_error) result` and enforces **well-definedness**:
  - A `Div` divisor must be provably `> 0`, otherwise `Bad_divisor`.
  - Every arithmetic dimension in a **return** type must be provably `≥ 0`, otherwise
    `TypeError "Dimension … may be negative"`. This is how `h - kh + 1` gets rejected when nothing relates
    `h` and `kh`.
- In argument position, a side condition that fails makes the match fail, so backtracking continues.
- In return position, an unknown integer parameter still yields a fresh dimension (step 0 behavior).

## Signatures now expressible

```ocaml
(* conv2d(x: [n, c, h, w], k: [o, c, kh, kw]) -> [n, o, h - kh + 1, w - kw + 1] *)
(* max_pool(x: [*B, h, w], s: int) -> [*B, h // s, w // s] *)
```

## Tests (`test_gaps.ml`, step 1)

- conv2d on concrete shapes gives `[2, 8, 28, 28]`.
- A kernel larger than the image by 2 is rejected (`-1`).
- Unrelated symbolic `h`, `kh` are rejected: the result may be negative.
- A symbolic image `h = s + 4` with a 3×3 kernel gives `s + 2`. The proof goes through the nonnegativity from step 0.
- Pool floors `33 // 2 = 16`, rejects stride `0`, and gives fresh dims for an unknown stride.
- `Sub`/`Div` in argument position, both matching and mismatching.

## Limitation found while testing

`h - kh + 1` is `0` when `kh = h + 1`. That's a valid natural, but PyTorch raises on it. Saying
"`kh ≤ h`" needs a *precondition*, which is the refinement step. The conv arithmetic by itself can't express it.
