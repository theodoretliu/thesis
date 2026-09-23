# Future work

Open work on the shape checker after steps 0–7, most important first. The first four items are carried over
from the earlier notes. The rest came up while designing how to check the bodies of variadic functions.

## Checking functions that take variadics

The target: inside `def f(x: [*B, d])`, check the body once, and have that result hold for every caller.

**Design: `B` is rigid in the body.** In the body, `B` is one unknown list that can't be changed (a skolem).
If the body checks under that rule, it is correct for every list a caller could pass. Callers are then checked
only against `f`'s signature, never its body.

**What the guarantee needs.** Every rule the checker applies to a list variable must stay true when the variable
is replaced by any concrete list of dims (a substitution lemma). The step 7 rules already meet this:

- `B` equals only itself.
- Indexing from the left is allowed only if no list variable precedes the index, and indexing from the right
  only if none follows it.
- `prod_B` and `rank_B` are only known to be `≥ 0`.

The solver never assumes a fact that fails for some `B`. Every assertion either defines a fresh variable
(`mk_int_var`, `add_to_solver`, `IntExpr` returns), says a dim is `≥ 0`, or adds a signature's `ensures`
(which is trusted). So the existing per-call checks are sound for this.

**Passing `B` on to other functions** already works one call at a time. `linear(x, w)` binds `linear`'s
own variadic to a list containing `B` and returns `[*B, p]`, which can go into the next call.

### 1. Body checker (missing)

`check_sig` checks a single call. Nothing yet checks a whole function body. The step 7 tests chain calls
by hand. A body checker would:

1. Create the rigid list and dim variables from the parameter annotations.
2. Assume the function's own `requires`.
3. Thread each call's result into the next call.
4. Prove the returned shape equals the declared return type, and prove the `ensures`.

It should reset the solver (`push`/`pop`) per function. Today the one global solver is never reset. That's
sound only because nothing adds assumptions. Once step 2 adds a function's `requires`, those must not leak
into unrelated checks.

### 2. Rank-guided unfolding of list variables

Batched matmul needs `[*C, m, k]`. Passing it `x: [*B, d]` needs `B = [*C, m]`, which fails when `B` is
empty. Rejecting that is correct (`test_gaps.ml`, "batched matmul needs an m that [*B, d] doesn't
provably have"). There are two ways to make such calls go through:

- **Annotate more structure:** `x: [*B, m, d]`.
- **Refinement plus unfolding:** allow `requires rank(B) ≥ 1`. Under that assumption, the matcher may split
  `B` into `[*B', b]`, where `B'` and `b` are new rigid variables, with `rank_B = rank_B' + 1` and
  `prod_B = prod_B' · b`.

The second option is much smaller than the full list unification proposed in step 7 (associative, with
Presburger length constraints). A body check only splits rigid variables where a refinement allows it, and
the existing backtracking matcher already handles the callee's own variables. Do this after the body
checker.

### 3. Empty-list axiom for `prod`/`rank`

`fresh_list` asserts only `prod_B ≥ 0` and `rank_B ≥ 0`. It should also assert `rank_B = 0 ⇒ prod_B = 1`.
Without it the checker is incomplete (it may reject correct programs), but not unsound.

### 4. Size-1 dims broadcasting against list variables

A size-1 dim always broadcasts, but a list variable broadcasting against 1s is currently rejected.

### 5. Higher-order functions

Passing functions as arguments (`vmap`, `map` over a batch dim) needs rank polymorphism in the style of
Remora. None of the above covers this.

## Frontend

Carried over from [README](README.md), in order:

1. A jaxtyping-string parser that emits `signature` values.
2. An intraprocedural walk over marked Python functions that threads `Dimensions`/`SymInt`/list vars
   through calls. This is the Python-facing side of the body checker (item 1).
3. A signature file for the torch/numpy ops in use.

## Other

- **Dtypes.** Not modeled at all.
- **Source-level names in diagnostics.** Messages name parameters and signatures but not Python variables or
  source locations.
- **`Any` modes.** Step 7 rejects composing two unknown shapes (strict). A frontend could offer lenient mode.

## Prior work to compare against

These citations are from memory. Check them before citing in the thesis.

- **PEP 646 `TypeVarTuple`:** the same rigid-in-the-body treatment of variadics, without arithmetic.
- **Remora** (Slepak, Shivers, Manolios, ESOP 2014): separate dimension and shape kinds, with shape
  concatenation in the types. The closest formal match to this checker.
- **Gradual Tensor Shape Checking** (Hattori et al.): the strict vs. lenient handling of unknown shapes.
