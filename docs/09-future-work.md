# Future work

Open work on the shape checker after steps 0–9, most important first. The first four items are carried over
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

Items 1–4 below are done in step 8 ([10-function-bodies.md](10-function-bodies.md)).

### 1. Body checker (done)

`check_fundef` checks a whole body once: rigid parameters, the function's `requires` assumed, each call's
result threaded into the next, and the return type and `ensures` proven. Each function runs in its own
solver scope (`Z3utils.scoped`), so its `requires` don't leak.

### 2. Rank-guided unfolding of list variables (done)

When `rank(B) ≥ 1` is provable, the matcher may split `B` into `[*B', b]` or `[b, *B']`. Batched matmul on
`x: [*B, d]` now checks under `requires rank(B) ≥ 1`.

### 3. Empty-list axiom for `prod`/`rank` (done)

`fresh_list` asserts `rank_B = 0 ⇒ prod_B = 1`, and a list variable with provable rank 0 is dropped.

### 4. Size-1 dims broadcasting against list variables (done)

1s broadcast against a list variable. A result shape is computed when the list side provably has at least as
many dims as there are 1s.

### 5. Higher-order functions

Passing functions as arguments (`vmap`, `map` over a batch dim) needs rank polymorphism in the style of
Remora. None of the above covers this.

## Frontend

**Done in step 9** ([11-frontend.md](11-frontend.md)): the frontend reads Python files annotated with
[jaxtyping](https://github.com/patrick-kidger/jaxtyping) shape strings. jaxtyping is the most widely used
shape-annotation syntax. It isn't limited to JAX: it works with PyTorch, NumPy and TensorFlow arrays, and it
replaced torchtyping. jaxtyping checks shapes at runtime. This checker would check the same annotations
statically, for every caller, so users write no new syntax.

```python
from jaxtyping import Float
from torch import Tensor

def f(x: Float[Tensor, "*batch n d"], w: Float[Tensor, "d p"]) -> Float[Tensor, "*batch n p"]:
    return x @ w
```

How the shape string syntax maps onto `entry`:

| jaxtyping | Meaning | Checker |
|---|---|---|
| `b`, `seq` | named dim, same size wherever it appears | `Id` |
| `3` | fixed size | `Int` |
| `*batch` | named run of zero or more dims | `Spread` |
| `...` | unnamed run of dims | a fresh `Spread` |
| `_` | one dim that isn't checked | a fresh `Id` |
| `*#b` | `b` again, broadcastable | `Broadcast` (a first occurrence binds `b`) |
| `#b` | `b`, or 1 (broadcastable) | not supported yet |
| `dim-1`, `2*dim` | arithmetic on dims bound earlier | `Add`/`Sub`/`Mul`/`Div`, restricted to `+ - * //`, names and ints (jaxtyping evaluates arbitrary Python) |
| `Float[...]`, `Int[...]` | dtype | ignored until dtypes are modeled |

**Refinements stay, but are a last resort.** `requires`/`exists`/`ensures` are post-thesis additions (step 5)
and jaxtyping has nothing like them. The checker keeps all three, but signatures and examples should be typed
without them wherever possible:

- **`exists`** is implicit. A name that appears only in the return annotation is existential, which is how
  jaxtyping already binds it.
- **`requires`** belongs in stubs, mainly `reshape`/`view` (`prod(A) = prod(B)`), divisibility
  (`d % heads == 0`), and bounded indices (`topk`, `narrow`). Prefer annotating more structure (`"*batch n d"`)
  or fixing the stub (e.g. `matmul` overloads for a 1-D side) over adding one. In stubs, `requires` and
  `ensures` are written as `assert`s in the body. In user code, asserts are dropped for now (sound). Assuming
  them after they run is the next step. Whether callers must also prove leading asserts is open.
- **`ensures`** only matters for bounds on data-dependent sizes (`unique` → `m <= n`). Avoid it unless a
  real program needs one.
- `conv2d`/pooling don't need `requires`: returned dims must already be provably `≥ 0`.

Any user-facing syntax beyond `assert`, such as a decorator, is not settled.

The architecture (Python frontend → JSON IR → OCaml CLI), operator desugaring, `.pyi` stubs, explicit
errors for unsupported constructs, and the `examples/pass`/`examples/fail` harness are as proposed. See
[11-frontend.md](11-frontend.md) and [frontend/README.md](../frontend/README.md).

Remaining frontend work, most useful first. [12-transformer-goal.md](12-transformer-goal.md) orders the
same work around one target, the Transformer, and adds what it needs (modules, instance dims, `-1`,
`transpose(i, j)`):

1. **Asserts as assumptions.** `assert n >= 0` or `assert x.shape[0] == n` should inform the rest of the
   body. (`torch.zeros(n)` for `n: int` now infers `requires n >= 0`; see
   [13-free-functions.md](13-free-functions.md).)
2. **`x.shape[i]`**, as an int equal to a dim, like `x.size(i)` (done in 13-free-functions.md).
3. **Control flow.** `if` needs a join of shapes (or both branches checked against the declared type), and
   loops need invariants.
4. **Classes.** `nn.Module` subclasses check, typed by their constructor's ints rather than annotations on
   `self` attributes ([14-modules.md](14-modules.md)). Modules as values (arguments, returns, locals) and
   subclassing user classes remain.
5. **Lists:** `torch.cat`, `torch.stack`. (Tuple returns and unpacking are done.)
6. **Single broadcastable dims (`#b`)**, and an in-place `Broadcast` ("broadcasts *to* A") for `x += y`.
7. **NumPy stubs**, and more of torch.

## Other

- **Dtypes.** Not modeled at all.
- **Source-level names in diagnostics.** Step 9 reports `file:line` and the statement's source. Callee
  existentials still print as solver names.
- **`Any` modes.** Step 7 rejects composing two unknown shapes (strict). A frontend could offer lenient mode.

## Prior work to compare against

These citations are from memory. Check them before citing in the thesis.

- **PEP 646 `TypeVarTuple`:** the same rigid-in-the-body treatment of variadics, without arithmetic.
- **Remora** (Slepak, Shivers, Manolios, ESOP 2014): separate dimension and shape kinds, with shape
  concatenation in the types. The closest formal match to this checker.
- **Gradual Tensor Shape Checking** (Hattori et al.): the strict vs. lenient handling of unknown shapes.
