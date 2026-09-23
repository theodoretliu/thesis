# Step 3: broadcast result shapes

**Gap:** two variadics don't unify under broadcasting semantics. `T[*Bs, M, K] @ T[*Cs, K, N]` has no
expressible type. `main` could *check* `Broadcast A` in an argument, but a broadcast in the return type
raised `TypeError "Broadcast in return type"`, so `x + y` had no result shape.

## Changes

- New dimension constructor **`Broadcasted [A; B; ...]`**: the numpy broadcast of already-bound spreads.
  It works in return and argument position. It can't appear under arithmetic (kind error).
- `broadcast_pair` aligns from the right. For each pair it proves `x = y ∨ x = 1 ∨ y = 1`. It reuses `x` or
  `y` when it can prove which case holds, and otherwise defines a fresh dim `ite(x = 1, y, x)`. So
  `d` broadcast with an `e` known only to be "1 or d" still gives `d`.
- Refactor: `Drop`, `Keep`, and `Broadcasted` are all *derived list dimensions*, computed by `derived_dims`
  and matched by `strip_equal_prefix`. This removes three copies of the prefix-equality loop, and step 4's
  list functions plug into the same place.

## Bug fixed along the way

`(Nparray (h :: t), Dimensions [])` returned `None` before trying `h`. As a result a spread could never capture
zero trailing dimensions: `f(x: [*A])` rejected 0-d arrays, and `[n, *R]` rejected `[n]`. This was on `main`.
It turned up because broadcasting with a scalar failed.

## Now expressible

```ocaml
(* x + y *)          ([X: [*A]; Y: [*B]],             [Broadcasted [A; B]])
(* batched matmul *) ([X: [*A, m, k]; Y: [*B, k, p]], [Broadcasted [A; B]; m; p])
```

For example `[2, 1, m, k] @ [5, k, p]` gives `[2, 5, m, p]`, and `[3, m, k] @ [4, k, p]` is rejected.

## Not covered

- The per-pair proof is conservative. An unconstrained pair `(a, b)` is rejected, not accepted under
  an assumed side condition. Recording such assumptions is part of the refinement step.
