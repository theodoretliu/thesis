# Step 8: checking function bodies

**Gap:** `check_sig` checks one call. Nothing checked a whole body, so `def f(x: [*B, d])` could only be
checked by chaining calls by hand (step 7), and a function's `requires` had nowhere to go.

## Design

This follows the design in [09-future-work.md](09-future-work.md): **`B` is rigid in the body.**

A body is a small straight-line IR, so the checker doesn't depend on a Python frontend:

```ocaml
type term = Var of string | Lit of int | Call of string * term list
type stmt = Let of string * term | LetAnnot of string * typ * term | Return of term
type fundef = { name : string; sg : signature; body : stmt list }

check_fundef : (string * callee) list -> fundef -> unit
check_program : (string * callee) list -> fundef list -> unit
```

`check_fundef env f`:

1. Opens a solver scope (`Z3utils.scoped`: `push`/`pop`, and it restores unfoldings).
2. Makes each parameter rigid (`rigid_param`). A new `Id` becomes a fresh dim, a new `Spread` a fresh list
   variable, an `int` a fresh unconstrained int, and arithmetic or derived entries get their values.
   Parameter dims are assumed `≥ 0`, because a caller's dims equal them.
3. Assumes `requires`, and rejects contradictory ones (otherwise every body would check).
4. Evaluates each statement. Calls go through `check_sig`/`check_overloads` against `env`. `LetAnnot`
   matches the value against the annotation, which can bind new names for later annotations.
5. Matches the returned value against `ret` with the parameters' bindings. The same matcher binds `exists`
   names. Then it proves `ensures`.

`check_program` checks functions in order and adds each one's *signature* to the environment, so callers
never see bodies. A body may call its own function, through its signature.

**Why it's sound.** Every assertion in the body's scope is either definitional (a fresh name for an
expression, an unfolding), a dim being `≥ 0`, a callee's `ensures` (trusted), or the function's own
`requires`, which every caller must prove. Nothing constrains a rigid variable beyond what holds for every
caller, so a body that checks is correct for every argument that satisfies the signature.

## Rank-guided unfolding

If `rank(B) ≥ 1` is provable, `B` may be split:

- **Right:** `B = [*B[:-1], B[-1]]`. The matcher tries this for an argument after its match fails. This lets
  batched matmul `[*A, m, k]` take `x: [*B, d]`.
- **Left:** `B = [B[0], *B[1:]]`. The matcher tries this when a single dim meets `B`, as in
  `shape0(x: [n, *R])`.

An unfolding defines fresh variables from `B` (`rank_B = rank_B' + 1`, `prod_B = prod_B' · b`), so it
assumes nothing about `B`. Unfoldings live in `Z3utils.unfoldings`. `expand` rewrites unfolded variables
into their parts everywhere a list is compared, so `B` and `[*B[:-1], B[-1]]` are the same shape. The
matcher undoes an unfolding if the rest of the match fails, so a failed branch can't commit `B` to one side.
Chains are capped at depth 8.

`expand` also drops a list variable whose rank is provably 0. Together with the new axiom
`rank_B = 0 ⇒ prod_B = 1` in `fresh_list`, `requires rank(B) = 0` makes `[*B, d]` equal to `[d]` and
`flatten([n, *B])` equal to `[n, 1]`.

## 1s against list variables

- **Check (`Broadcast`):** 1s broadcast with a list variable of any length.
- **Result (`Broadcasted`):** a list variable against 1s gives the list side unchanged, but only if the list
  side provably has at least as many dims as there are 1s. `[*B, d] + [1, d]` is `[*B, d]` under
  `rank(B) ≥ 1`. It's rejected otherwise, because for `B = []` the result is `[1, d]`.

## Diagnostics

Errors name the function and the statement, then the callee, then the step 6 message:

```
in mlp, `return linear(z, w1)`: linear: Could not type check: parameter W (array[k, p], given array of shape [d, h]): expected k = h, got d
in f, `return head(x, y)`: head: Precondition not provable: m <= n
in f, `return unique(x)`: Postcondition not provable: k = var69 < n
```

Rigid dims print with their parameter names (`Z3utils.dim_labels`), and unfolded parts print as `B[-1]` or
`*B[:-1]`. `name = value` collapses to `name` when the value is just that name.

## Tests

`checker/test/test_bodies.ml` covers the MLP example, rigidity (`return x` as `[d]`, `sum(x, 0)`), annotations,
int parameters, `requires`/`ensures`/`exists`, unfolding (batched matmul, `B`'s first dim, exact ranks, and
the negative case that `rank(B) ≥ 1` doesn't make `B` two dims), 1s broadcasting, and a two-function program.
`SHOW_ERRORS=1 dune exec test/test_bodies.exe` prints every diagnostic.

## Limitations

- **No control flow.** The IR is straight-line. `if` needs a join of shapes, and loops need invariants.
- **One side per unfolding.** If `B` has been split on the right and a match later needs its first dim,
  that needs `rank(B) ≥ 2`. Undoing unfoldings on failure keeps this from happening across branches.
- **Existential dims from callees print as solver names** (`var69` above).
- **Python-level constructs** such as `x.shape[i]`, tuples, and methods need signatures or frontend support.
