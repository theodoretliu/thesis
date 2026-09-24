# Transformer milestone 1: the free functions

**Gap:** none of the Transformer's three free functions checked (see
[12-transformer-goal.md](12-transformer-goal.md)). `subsequent_mask` needed a dim named after an int parameter
and `torch.ones(1, size, size)`, `make_masks` needed tuple returns, `x.size(i)` and `&`, and `loss` needed `-1`
in `reshape` and `F.cross_entropy`. `subsequent_mask`, `make_masks` and `loss` now check (3 of 24).

## Decisions

These follow the recommendations in 12-transformer-goal.md:

- **Int parameters in shapes (G3).** In `subsequent_mask(size: int) -> Bool[Tensor, "1 size size"]`, the dim
  `size` *is* the int. The frontend no longer renames an int parameter apart from a dim with its name. Other
  parameters still are (`n` becomes `n'`). An int parameter may come after the arrays whose shapes use it,
  so the core binds int parameters first.
- **Ints used as sizes (G8): inferred preconditions.** torch raises on `torch.ones(-1)`, so rather than
  reject `torch.ones(n)` for `n: int`, the checker infers `requires n >= 0`, and callers must prove it.

Checking `loss` settled a question the decisions didn't cover. torch can't infer a `-1` when the other sizes
multiply to 0 (`torch.zeros(2, 3, 0).reshape(-1, 0)` raises), so `logits.reshape(-1, vocab)` fails at run time
if `vocab` is 0. Applying decision 3's principle, the checker infers `vocab >= 1` and moves the runtime
error to the caller.

## Inferred preconditions

With inference on, a body that can't prove a size is valid may assume *sign facts* about its parameters: an
int parameter is `>= 0` or `>= 1`, and a dim is `>= 1`. The checker assumes the fewest facts that make the
proof go through. It tries dropping each candidate, stronger ones first, so `n >= 0` is kept over `n >= 1`.
Each fact it assumes is added to the function's `requires`.

Only size obligations can be discharged this way:

- an int used as a shape entry is `>= 0` (`torch.ones(n)`)
- a returned dim is `>= 0` (`def f(n: int) -> "n-1"`)
- the other sizes of a `-1` have a positive product
- a callee's requires that is itself a sign fact holds. This is how inferred requirements pass up to
  callers: `sm(k)` infers `k >= 0`, `sm(k - 1)` infers `k >= 1`, and `sm(k - 2)` is rejected.

Relations must still be proved. conv2d's `kh <= h + 2 * padding` isn't inferred as `h >= 1`, so
`examples/fail/conv_kernel_fit.py` still fails. Neither is `-1`'s divisibility.

Callers need the inferred requires, so the checker CLI runs rounds. Each round checks every body with
inference on, against the signatures plus what earlier rounds inferred. It stops at the first round that
infers nothing new, whose results are then those of an ordinary check. Facts only accumulate, and each
round adds at least one of finitely many, so this terminates. After 20 rounds, a final check with
inference off decides. The CLI reports each function's inferred requires (`"inferred": ["n >= 0"]`), and
the frontend prints them as notes:

```
examples/goal/transformer.py:48: note: subsequent_mask requires size >= 0 (inferred from its body)
examples/goal/transformer.py:269: note: loss requires vocab >= 1 (inferred from its body)
```

**Overloads.** An overload is tried inside a solver scope. If it fails, its assertions, unfoldings,
pending `-1`s and inferred facts are all undone. The overloads are first tried with inference off, so one
that needs no new requires is preferred. `Z3utils.scoped` now pops back to its own level, which drops the
scopes that successful attempts leave pushed.

## `-1` in `reshape` and `view` (G13)

A `-1` in a shape (`Shape` term) becomes a fresh *pending* dim. The callee's requires determines it: after the
arguments match, an `Eq` whose sides are `total` and `x * rest` (reshape's `prod(A) = prod(B)`) defines `x`.
torch's conditions are checked as it states them:

- `rest > 0`, a size obligation, so it can be inferred (`the other sizes' product b * 3 * 4 may be 0`)
- `rest` divides `total`, which must be proved (`b * t may not be divisible by 3`)

Nonlinear `mod` goals are often too hard for Z3: `(b * n * 12) mod (b * 12) = 0` hung. So the quotient is
built by cancelling factors first. Each factor of `rest` must provably equal a factor of `total`, or be a
number dividing the product of `total`'s numbers. So `b * n * 12 / (b * 3 * 4) = n`. Only when that fails
does the checker ask Z3 for `mod`. A pending dim that no equation determines is an error
(`torch.zeros(-1)`: `the size -1 can't be inferred here`), and so is a second `-1`.

The solver now has a 2 s timeout per query. A goal that times out is unknown, so it isn't proved and fails
cleanly instead of hanging.

## Other core and frontend changes

- **`Index (A, i)`**, one dim of a bound spread, usable in arithmetic. Stubs write it `A[i]`, as in
  `def size(self: "*A", dim: int) -> Dim["A[dim]"]`. It's undefined if `i` is out of range or a list variable
  is in the way.
- **Tuples (G6):** a `TypeTuple` return type (`tuple[A, B]` in user code), `Tuple` values, a `Tup` term for
  `return a, b` and tuple expressions, and `Unpack` for `a, b = f(x)`. A tuple's elements are matched in
  order, and errors name the element (`return value[0]`). Tuple parameters and annotations aren't supported.
- **Bitwise operators:** `&`, `|`, `^` and `~` go to `operator.and_`, `or_`, `xor` and `invert`. On
  arrays they broadcast like arithmetic.
- **Stubs:** `Tensor.size`, `torch.triu`/`tril` (as functions and methods), and `F.cross_entropy` for class
  targets (`[c]` with `[]`, or `[n, c, *D]` with `[n, *D]`, reduced to a scalar). `ignore_index` and
  `label_smoothing` are keyword-only, so an unstubbed positional argument like `weight` is rejected.
- **Labels:** an int a call returns is labeled by its value, so `x.size(-1)` prints as `vocab` rather
  than `var12`, and an inferred `-1` prints as its quotient.

## Soundness notes

- An inferred fact is assumed only while checking the body that needs it, and it becomes that function's
  requires. So every caller proves it, or infers it in turn.
- A round that inferred something is never the final result. Its bodies assumed facts that their
  callers hadn't seen yet.
- A pending `-1` is defined only by an equation the callee requires, after the other sizes are proved
  positive and to divide the total. The definition is the unique solution.
- The factor-cancelling quotient relies only on proved equalities and exact integer division.

## Tests

- `checker/test/test_bodies.ml`: `x.size(i)` and out of range; `-1` needing nonzero sizes, then checking
  given them, the inferred `b >= 1`, divisibility, and a second `-1`; `n >= 0` inferred and propagated; a
  relation not inferred; an int parameter after the shapes that name it; tuple returns and element errors,
  unpacking, and tuple parameters rejected.
- `frontend/tests/test_translate.py`: int parameters as dims, tuple return types and their errors, `A[i]`
  in stubs, unpacking, and bitwise operators.
- `frontend/tests/test_examples.py` pins the preconditions inferred for `examples/pass/sizes.py`.
- `examples/pass/sizes.py`: a causal mask, masks returned as a tuple and unpacked, `split_heads` with `-1`,
  and a token loss. `examples/fail/negative_size.py` now shows a caller that can't meet an inferred
  requires, and `examples/fail/infer_size.py` shows an indivisible `-1`.
- `frontend/tests/test_goal.py`: `PASSING` is `subsequent_mask`, `make_masks`, `loss`.

## Limitations

- Inference only knows sign facts with bounds 0 and 1. `torch.zeros(n - 2)` isn't inferred as `n >= 2`.
- Only a literal `-1` is inferred, and only through an equation in the callee's requires.
- `x.shape[i]` still isn't translated. `x.size(i)` is.
- `cross_entropy` has no `reduction` or probability targets.
