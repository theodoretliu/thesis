# Step 7: symbolic variadic arguments (function bodies, `Any`)

**Gaps:**
- `Any` leaks: unshaped values compose silently.
- The thesis's "Restrictions on Argument Types": arguments had to have a known number of dims, so function
  bodies like `def f(x: [*B, d])` couldn't be checked, because `x`'s shape contains an unknown list.

## Design

A **list variable** is a symbolic name for an unknown-length run of dims. It lives in `Dimensions` next to
dim vars (`Z3utils.fresh_list ~label:"B" ()`), so the matcher's types don't change. Inside
`def f(x: [*B, d])` the argument is `Dimensions [B; d]`. **`Any` is `[*fresh]`** (`unknown_shape ()`): a
completely unknown shape is one list variable. It works wherever a signature is generic over it and gets
rejected wherever a proof would depend on its length.

The rules are all conservative, meaning they reject rather than guess:

| Construct | Rule for list variables |
|---|---|
| `Spread` | captures list vars like any other item (splits are over items) |
| equality of dims | a list var equals only itself |
| `Id`/`Int`/arithmetic | can't match a list var, since it might be empty or long (`"… has an unknown number of dimensions"`) |
| `Broadcasted`, `Broadcast` | a list var broadcasts only with itself, or against an exhausted side |
| `Drop`/`Keep`/`SetAt`/`InsertAt` | index `i ≥ 0` is allowed only if no list var precedes it. Index `i < 0` is allowed only if none follows it. |
| `Permute` | no list vars |
| `Prod`, `Rank` | a list var contributes solver terms `prod_B`, `rank_B ≥ 0` |

## Now checkable (tests in `test_gaps.ml`, step 7)

With `x: [*B, d]`:
- `sum(x, -1)` gives `[*B]`. `sum(x, 0)` is rejected.
- `linear(x, w[d, p])` gives `[*B, p]`.
- `x + x` and `x + bias[d]` give `[*B, d]`. `x + y[*C, d]` is rejected.
- `unsqueeze(x, -1)` gives `[*B, d, 1]`. `permute` is rejected.
- `flatten([n, *B])` gives `[n, prod(B)]`, which is consistent across calls. Passing it to `linear(6272)` is rejected.
- `rank([*B, d]) = rank(B) + 1`.

With `u = Any`: `sum(u)` and `u + u` are fine. `u @ w` and `u + other_any` are **rejected**, not silently
accepted. This is the "refuse to compose `Any`" behavior. A frontend can offer it as strict mode and accept these cases in lenient mode.

## Limitations

- No list *equations*. The checker won't learn `B = [*B', m]` from a successful match, so `[*B, d]` never
  satisfies a signature that needs an explicit dim inside `B`, such as batched matmul's `m`. Solving that
  needs list unification (associative, with Presburger length constraints), which is a bigger design step.
  Step 8 handles the batched matmul case under `requires rank(B) ≥ 1` by unfolding `B`
  ([10-function-bodies.md](10-function-bodies.md)).
- A list var broadcasting against dims of 1 is rejected even though `1`s always broadcast. (Fixed in step 8.)
