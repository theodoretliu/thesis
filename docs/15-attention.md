# Transformer milestone 3: attention

**Gap:** `attention` and `MultiHeadedAttention.forward` take an `Optional` mask and branch on it (G4), pass a
module as an argument, split `d_model` into `h` heads of `d_model // h` (G7), broadcast the mask with `*#b`
and `#q` (G11, G12), and transpose axes 1 and 2 (G14). See [12-transformer-goal.md](12-transformer-goal.md).
Now 17 of 24 check. `attention`, `MultiHeadedAttention.forward`, `EncoderLayer.forward` and
`DecoderLayer.forward` are new. The two layers only needed `MultiHeadedAttention.forward`'s signature.

## Decision: `Optional` is checked once per case

Decided 2026-09-25: a function is checked once for each choice of which `Optional` parameters are `None`.
The alternative was an `Optional` type in the core, with narrowing on `is not None` and a join of the
branches' types afterwards.

At a call, the frontend always knows whether an argument is `None`: it's the literal, a default of `None`,
a local assigned `None`, or an `Optional` parameter that's `None` in the case being checked. Anything
else isn't `None`. So:

- **Each case is a checker function of its own.** `attention` has four: `attention`,
  `attention[mask=None]`, `attention[dropout=None]` and `attention[mask=None,dropout=None]`. A `None`
  parameter isn't in that case's signature at all, so its shape binds nothing.
- **`if mask is not None:` is decided statically** in each case, and only the branch taken is translated.
  So is `x if y is None else z`. The tests may use `is None`, `is not None`, `not`, `and` and `or`. These
  short-circuit, so a later operand is only translated in the cases that reach it. Any other `if` is
  still an error.
- **A call goes to the case its arguments select.** `attention(q, k, v, mask=mask, ...)` in
  `MultiHeadedAttention.forward` calls `attention` in `forward`'s own case and `attention[mask=None]` in
  `forward[mask=None]`.
- **A function passes if every case does.** Errors name the case (`in masked[mask=None]: `mask` is None
  here`), and an error shared by several cases is reported once. Inferred requires are per case.

No join is needed, since each case is straight-line code: `mask = mask.unsqueeze(1)` in the branch just
rebinds `mask`. The cost is 2ⁿ checks for n `Optional` parameters, at most 4 here. The frontend allows at
most 4 `Optional` parameters per function (16 cases).

`Optional[T]`, `Union[T, None]`, `T | None` and their string forms are recognized. `Optional` isn't
supported in stubs, on `__init__` (its attributes would depend on the case), or in return types.

## Modules as values

A parameter annotated with a module class (`dropout: nn.Dropout`, or a user's `nn.Module` subclass) is an
instance, lowered like `self`: it's its instance dims, as int parameters named `dropout.p` for `Dropout`'s
`p`. Its signature lists the instance, so the body and callers assume the constructor's invariant. An
argument must be an instance of that class: `self.dropout`, `self`, or another module parameter. In the
body, `dropout(x)` calls `forward` and `dropout.m(x)` calls a method.

`nn.Dropout` has no instance dims, so `attention`'s `dropout` adds no parameters.

## Asserts (G7)

An assert used to be dropped. Now the rest of the body assumes it, through a new statement
`Assume c` for a constraint `c` over the body's int locals:

- **What's stated:** comparisons of ints (`==`, `<`, `<=`, `>`, `>=`, chained) with `+ - * //`, and
  `a % b == 0`, which is written `b * (a // b) == a`. A call or attribute in one, like `x.size(1)`, is
  evaluated first into a local named after its text, `(x.size(1))`. Conjuncts that can't be stated
  (`x is not None`, `a != b`, floats, `a % 2 == 1`) are dropped, which is sound.
- **Divisors:** a quotient can only be stated when its divisor is positive. With inference on, the
  divisor may be inferred positive like a size: in `MultiHeadedAttention.__init__`,
  `assert d_model % h == 0` infers `h >= 1`. Python allows negative divisors, so this is stronger than
  the runtime needs. But `h` is later a size in `view`, where torch rejects a negative one. Nothing is
  inferred for a fact that would be dropped anyway, e.g. one about a tensor.
- **In `__init__`**, an assert about the instance dims holds for every instance. So the frontend adds it
  to the constructor's `ensures`, which the body proves (it assumed it), and the class invariant is now
  the constructor's requires, then its ensures. Only facts whose divisors are instance dims or positive
  numbers are lifted, since those can be stated once the requires are assumed. A call's `ensures` with a
  divisor the caller can't prove positive is dropped, like an invariant's.

The invariant is now assumed before a body's array parameters are made rigid, and before a call's
arguments are matched. It's about the int parameters, so a shape like `"b h n d_model//h"` can rely on it.

## Broadcasting (G11, G12)

**`*#b` broadcasts to `b`**, as in jaxtyping (decision 4 of the goal): right-aligned, the dims must be no
more than `b`'s, and each one is 1 or equal to `b`'s. The core used to check that the two broadcast
*with* each other, which accepted a mask with more dims than `b`. jaxtyping rejects that at run time.

- At a call, a `*#b` entry tries the ways the argument can split around it, with the one that can fit
  tried first, so a mismatch is reported where it is: `[b] doesn't broadcast to *b = [b, h]`.
- In a body, a `*#b` parameter is a fresh list variable known to broadcast to `b`'s dims, with rank at
  most theirs. It broadcasts to any shape that ends with `b`'s dims, so the mask in `attention` can be
  passed to `masked_fill(mask: "*#A")` against `[*b, q, m]`.

**`#q` is `q` or 1** (`BroadcastDim`). In a body, it's a fresh dim `d` with `d = q ∨ d = 1`. At a call,
the argument's dim must provably be one of them. Like a dim, `#q` binds `q` where it first appears, so an
unbound `q` is bound to the argument's dim, one of the values `q` may take. It can't be in a return type.

The new stub `Tensor.masked_fill(self: "*A", mask: "*#A", value: float) -> "*A"` uses `*#A`.

## `transpose(i, j)` for any literal axes (G14)

The list function `*swap(A,i,j)` exchanges two dims. Each index must resolve to a dim, but list
variables may sit between or around them: `[a, *L, b]` swapped at 0 and -1 is `[b, *L, a]`. The four
`transpose` overloads for `(0, 1)` and `(-2, -1)` are now one:

```python
def transpose(self: Shaped[Tensor, "*A"], dim0: int, dim1: int) -> Shaped[Tensor, "*swap(A,dim0,dim1)"]: ...
```

## `-1` from the class invariant

`q = self.w_q(query).view(nbatches, -1, self.h, self.d_k)` needs the `-1` to be `q`, from
`b * q * d_model = b * x * h * (d_model // h)`. Milestone 1 cancelled factors pairwise and otherwise asked
Z3 for `mod`, which times out on this. Two more ways to cancel, tried in order:

- **Grouped:** the unmatched factors of the other sizes multiply to some of the total's, fewest first:
  `h * (d_model // h) = d_model` by the invariant, so the `-1` is `q`. This also gives
  `view(nbatches, -1, self.h * self.d_k)`, where one size `h * d_k` equals two of the total's.
- **Divided:** each unmatched factor `u` is a number or a positive divisor of one of the total's factors
  `t` (`u * (t // u) = t`), which becomes `t // u`. So after `assert x.size(1) % k == 0`,
  `x.reshape(b, k, -1)` is `[b, k, n // k]`.

`MultiHeadedAttention.forward` infers `b >= 1, d_model >= 1`: torch can't infer a `-1` when the other
sizes multiply to 0. The layers' `forward`s infer the same in turn.

## Soundness notes

- **Cases:** every call passes `None` or not for each `Optional` parameter, and the frontend knows which,
  so every call is covered by a case that was checked. A branch not taken in a case is unreachable there.
- **Asserts:** the statements after an assert only run if it passed. A divisor inferred positive becomes
  a requires, which callers prove. Dropping a conjunct only assumes less.
- **Constructor ensures:** the body proves them at every return, and every instance was built by a
  constructor call that returned. `__init__` can't reassign its int parameters, so an assert about them
  still holds at the end.
- **`*#b` in a body:** the list variable stands for any shape that broadcasts to `b`. The only thing
  concluded from it is that it broadcasts to a shape ending with `b`'s dims, which follows.
- **`#q` at a call:** binding an unbound `q` to the argument's dim picks one value of `q` the body was
  checked for.
- **Quotients:** grouped and divided cancellation use only proved equalities, a proved positive divisor,
  and the other sizes' product proved positive, so the `-1` is the unique solution.

## Other changes

- **Diagnostics:** an int the checker knows nothing about prints as `?` rather than `var17`. A `None`
  local passed to an operator or stub is `mask is None here`, rather than a binding error per overload.
- **CLI:** `passed` in the frontend's report lists Python functions that check in every case.

## Tests

- `checker/test/test_bodies.ml`: `#n` accepts `n` or 1 and binds an unbound `n`; `*#B` accepts fewer
  dims and 1s but not more dims; a body's `*#B #n m` mask passes on and broadcasts to `[*B, n, m]` and
  `[*C, *B, n, m]` but not another spread; `swap` at `(1, 2)`, past and around a spread, and an ambiguous
  index; an assert assumed, its divisor inferred, a tensor assert dropped without inferring; split and
  merge heads with `-1` and the invariant; `-1` after an assert that `k` divides `n`. The old test that
  `[1, 1]` broadcasts to any `[*B]` now needs `rank(B) >= 2`.
- `frontend/tests/test_translate.py` (`Optionals`, `Asserts`): the cases and their signatures, statically
  decided `if`s, calls selecting a case, passing an `Optional` on, `T | None`, `None` locals, module
  parameters, errors reported once, the limit of 4, and asserts stated, dropped and lifted into `ensures`.
- `examples/pass/masks.py`: a masked softmax with an `Optional` mask and dropout, self-attention with
  heads, and `fold` with an assert. It also runs under jaxtyping's runtime checker, which rejects a mask
  of higher rank than `*b` and a `#q` that's neither `q` nor 1. `examples/fail/mask_heads.py` forgets
  `unsqueeze(1)` for the heads, `examples/fail/optional_none.py` uses a mask that may be `None`, and
  `examples/fail/heads_indivisible.py` drops `assert d_model % h == 0`.
- `frontend/tests/test_goal.py`: `PASSING` has 17 functions.

## Limitations

- **Optional:** at most 4 per function. Not on `__init__`, in stubs, or as return types. Only locals can
  be tested for `None` (not `self.x is None`), and a call's result is never `None` as far as the checker
  knows.
- **Module parameters:** an annotation can't name a module parameter's dims, so a `lin: nn.Linear`
  parameter can only be applied to arrays whose shapes don't need to match its dims. Modules still can't
  be returned or stored in locals.
- **Asserts:** only int comparisons, and `%` only as `a % b == 0`. Divisors are inferred positive, not
  just nonzero.
- A body's `*#b` only broadcasts to shapes ending with `b`'s dims, or with itself. `mask & other` of two
  such masks isn't supported.
