# nanoGPT milestone 3: causal attention

**Gap:** `CausalSelfAttention.forward` ([17-nanogpt-goal.md](17-nanogpt-goal.md)) unpacks `x.size()` (N2),
splits the fused projection into `q, k, v` (N3), branches on `self.flash` and reads the mask buffer that
only the slow path registers (N4 in bodies), passes `self.dropout if self.training else 0`, fills with
`float("-inf")` (N9), and slices the mask to `[:T, :T]`, which broadcasts against the scores only if
`T <= block_size` (N12). Now 8 of 11 check: `CausalSelfAttention.forward` is new, and `Block.forward` now
requires what it does. This took stubs, three frontend changes, and one new kind of inferred
precondition in the core.

```
gpt.py:69: note: CausalSelfAttention.forward requires n_embd >= 1, t <= block_size (inferred from its body)
gpt.py:128: note: Block.forward requires n_embd >= 1, t <= block_size (inferred from its body)
```

`forward` has three cases: `[self.flash=True,self.training=True]`, `[self.flash=True,self.training=False]`
and `[self.flash=False]`.

## `x.size()` and `split`: an overload per count

A tuple's length is fixed in the checker, but `x.size()` has as many entries as `x` has dims, and
`x.split(s, dim)` returns `ceil(x[dim] / s)` pieces. Both are stubs with an overload per count:

```python
@overload
def size(self: Shaped[Tensor, "a b c"]) -> tuple[Dim["a"], Dim["b"], Dim["c"]]: ...

@overload
def split(self: Shaped[Tensor, "*A"], split_size: int, dim: int = 0) -> tuple[
    Shaped[Tensor, "*setat(A,dim,split_size)"],
    Shaped[Tensor, "*setat(A,dim,split_size)"],
    Shaped[Tensor, "*setat(A,dim,A[dim]-2*split_size)"],
]:
    assert split_size >= 1 and 2 * split_size < A[dim] and A[dim] <= 3 * split_size
```

The checker takes the first overload that accepts the arguments. A rank matches one `size` overload, and
the asserts hold for exactly one number of pieces, so the call's tuple has the right length or none, and
unpacking checks that it's the number of names. `size()` has overloads for ranks 1 to 6, and `split` for
1 to 4 pieces. `split_size >= 1` comes first, because it's a sign fact, which a caller may infer (as
`n_embd >= 1` here). The other two are relations, which must be proved, and are provable once it holds.

### Alternatives

- **A signature made per call**, from the number of names unpacked. It would give a better error ("can't
  split into 2") and no limit, but it's a special case in the frontend for each such function, and
  `split` couldn't be called without unpacking.
- **`x.size(0)`, `x.size(1)`, ... for `B, T, C = x.size()`.** That doesn't check that `x` has exactly
  three dims, which Python's unpacking does.

## Attributes assigned in a branch

nanoGPT registers the mask under `if not self.flash:`. Milestone 2 required an attribute to be assigned
once, at the top level of `__init__`, so methods couldn't read it. Now an attribute may also be assigned
inside `if`s: each assignment is recorded with the tests around it (`fields_of`, `Guarded`), and where a
method reads the attribute, the tests are decided in its case, as `__init__`'s names (`field_value`).
Deciding them may split the case, like any test of a flag. Exactly one assignment must run:

- In `forward[self.flash=False]`, `self.bias` is the buffer.
- In a case where none runs, reading it is an error, since Python would raise `AttributeError`:

  ```
  mask_case.py:18: in Masked.forward[self.fast=True]: `self.mask` isn't assigned in this case: `Masked.__init__` assigns it under `if not self.fast:`
  ```

- Where more than one runs (at the top level and in a branch), reading it is an error: `` `self.b` is
  assigned more than once in this case ``.

`if bias: self.b = ... else: self.b = None` works the same way, so it's an alternative to milestone 2's
`... if bias else None`. A flag attribute must still be assigned once at the top level.

## `self.training`

`nn.Module.training` is a flag of every module, named `self.training`, unless `__init__` assigns an
attribute of that name. `train()` and `eval()` set it between calls, so a call is in one case. Tested,
it splits the case, and used as a value (`F.dropout(x, p, self.training)`) it's its case's `0` or `1`.
In nanoGPT, only the flash path tests it.

## Inferring `T <= block_size`

The manual path slices the mask to `self.bias[:, :, :T, :T]`, where the buffer is `[1, 1, block_size,
block_size]`. A slice clamps its stop, so each dim is `min(T, block_size)`, and `masked_fill` needs the
mask to broadcast to `[B, nh, T, T]`, which holds only if `T <= block_size`. Decision 3 of the goal is to
infer that, so nanoGPT is unchanged.

Inferred preconditions were sign facts only: `n >= 0` for an int, and `d >= 1` for a dim, tried where a
size must be valid. A relation isn't a fixed candidate, and the obligation that needs it isn't at the
slice but at a later call (here `masked_fill`), which uses plain proving. So:

1. **A slice registers a relation.** When the stop is known to be non-negative, but not to be within its
   dim, and each is provably equal to one of the signature's names, the slice records the relation
   `stop <= dim` over those names, here `t <= block_size`. It's stated over the names' values, which
   outlive the statement, not the slice's own dims. The slice's length is still `min`, so nothing is
   assumed yet.
2. **A statement that fails is checked again, assuming it** (`with_relations`). If a `Let`, unpacking,
   annotation or `return` fails with relations registered and not yet inferred, it's rolled back and
   checked again assuming each one, then all of them. If that checks, what it assumed is an inferred
   requires, which holds for the rest of the body, including after a loop whose body inferred it. A relation that contradicts what's known isn't assumed, as everything would be provable.
3. **Callers pass it on.** Inferred requires are now separate from declared ones in a signature
   (`inferred`). A callee's inferred relation `x <= y` may be inferred by a caller whose names equal both
   sides, so `Block.forward` requires `t <= block_size` too. `GPT.forward` (milestone 4) will prove it
   from its assert, as `GPTish` does in `examples/pass/causal.py`. A declared relation, like conv2d's
   kernel fitting the image, must still be proved.

The rounds of the CLI are unchanged: an inferred relation is one more requires of the function's group.

A slice whose stop may be past the end, where nothing needs it within, infers nothing, so code that
relies on clamping keeps its weaker signature.

### Alternatives

- **Infer the relation at the slice**, whenever the stop isn't known to be within bounds. It's simpler,
  but a function that relies on clamping, like `x[:k]` where `k` may be larger, would require `k <= n`
  of its callers for no reason.
- **An assert in the manual branch**, `assert T <= self.config.block_size`, as the Transformer has for
  `pe`. That changes nanoGPT (decision 3).
- **Inferring every relation between names**, declared ones too. That changes what's reported for
  existing code: `fit`'s kernel relation, a test in `test_bodies.ml`, would be inferred rather than
  reported.

## Soundness notes

- **`size()` and `split`.** An overload is only taken when its signature checks, including its asserts,
  so the tuple has as many entries as the call returns. A tensor with an empty split dim is rejected
  (torch returns one empty piece), which is only stricter.
- **Branch attributes.** An attribute's value in a case is the one assignment whose tests hold, decided
  by the same flags the case gives. Every instance is in some case, and in it exactly one assignment ran,
  or the read is rejected.
- **`self.training`** doesn't change during a call unless the method calls `train()` or `eval()`, which
  the checker doesn't support. Both values are checked.
- **Inferred relations** are requires: every caller proves them, or requires them in turn. A statement is
  only checked assuming a relation after it failed without one, and the retry is rolled back if it
  fails. A relation is only assumed if it's consistent with what's known.
- **`float(...)`** of a constant is a float, as Python's is.

## Other changes

- **Core:** `attempt` pops the solver back to where it started, not one scope, since a check that fails
  may have left scopes from overloads that succeeded before it. That matters now that a whole statement
  is attempted. A dim computed by a list function (`*setat(A,dim,A[dim]-2*split_size)`) is named by its
  value in errors rather than `var14`.
- **Stubs:** `Tensor.size()` per rank, `Tensor.split` per number of pieces, and
  `F.scaled_dot_product_attention` with an `Optional` `attn_mask`, `dropout_p` and `is_causal`.
- **`float`** of a constant is a float (`float("-inf")`). `float` of anything else is an error.

## Tests

- `checker/test/test_bodies.ml`: a slice's stop inferred within its dim when the return needs it, not
  when nothing does, for a later call in the statement (a causal mask), checking given it, inferred in a
  loop's body and holding after it, and a callee's
  inferred relation inferred in turn but not assumed without inference. The existing test that a
  declared relation isn't inferred still passes.
- `frontend/tests/test_translate.py`: `BranchAttributes` checks the cases of a module with a flag
  attribute, `self.training`, and an attribute assigned in both branches, the buffer read in its case,
  `float("-inf")`, `self.training` as a value, `scaled_dot_product_attention`'s case without a mask,
  the `size()` and `split` unpacking, and the errors: the buffer read in the wrong case, an attribute
  assigned at the top level and in a branch, `float` of a non-constant, and another module's `training`.
- `examples/pass/causal.py`: nanoGPT's attention with plain ints, a gated unit that splits in two, a block
  that passes the relation on, and a model that proves it from an assert. `test_examples.py` pins what
  each `forward` infers. It also runs under jaxtyping's runtime checker, on both paths and in both modes,
  and the assert fails past `max_len`. `examples/fail/split_count.py` unpacks three pieces into two
  names, `mask_case.py` reads the mask on the fast path, and `mask_length.py` calls a module whose mask
  covers 8 positions with 16.
- `frontend/tests/test_gpt_goal.py`: `PASSING` has 8 methods.

## Limitations

- **Counts are bounded:** `size()` up to rank 6, `split` up to 4 pieces. `x.size()` of a tensor whose rank
  isn't known (`*b d`) matches no overload, even when it's unpacked into names.
- **`split` with a list of sizes**, `chunk`, and `unbind` aren't stubbed.
- **Inferred relations are between two names**, `x <= y`: `t <= block_size - 1` or `t + 1 <= n` aren't
  inferred. Only a slice's stop registers one, not its start.
- **Branch attributes** need tests that are decided in each case: flags, and whether an attribute is
  `None` (`if self.x is None:`). A test on a parameter of `__init__` that isn't a flag, like `if mask is
  None:`, is an error where the attribute is read. An attribute assigned in a loop is still an error.
- **`self.training`** is a flag of `self` only, not of a nested module.
- **`float`** only of constants; `float(n)` of an int isn't supported.
