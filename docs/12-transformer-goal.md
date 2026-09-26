# Goal: check the Transformer

**Target:** [`examples/goal/transformer.py`](../examples/goal/transformer.py), the model from "Attention Is
All You Need" as a user would write it: PyTorch `nn.Module`s with jaxtyping annotations. It follows the
paper: post-norm residual blocks, scaled dot-product and multi-head attention, position-wise feed-forward
layers, scaled embeddings tied to the output projection, sinusoidal positional encodings, and the
label-smoothed loss.

The spec is correct code. `examples/goal/run_transformer.py` trains a step at two sizes (one is the paper's
base config) under jaxtyping's runtime checker, and checks that the runtime checker catches a bad mask.

```sh
uvx --python 3.12 --with torch --with numpy --with jaxtyping --with beartype \
    python examples/goal/run_transformer.py
```

**The spec** is `frontend/tests/test_goal.py`:

- `test_goal` checks that all 24 annotated functions and methods check. It was an expected failure until
  milestone 4.
- `test_progress` is a ratchet over `PASSING`. It fails if a function stops checking, or if one starts
  checking and isn't recorded.

**Progress:** 24 of 24: the goal is met. Milestone 1 is done ([13-free-functions.md](13-free-functions.md)):
`subsequent_mask`, `make_masks` and `loss` check. Milestone 2 is done ([14-modules.md](14-modules.md)):
`PositionwiseFeedForward`, `Embeddings`, `MultiHeadedAttention.__init__`, `EncoderLayer.__init__`,
`DecoderLayer.__init__` and `Transformer.encode`/`decode`/`forward` check. Milestone 3 is done
([15-attention.md](15-attention.md)): `attention`, `MultiHeadedAttention.forward` and the layers'
`forward`s check. Milestone 4 is done ([16-stacks.md](16-stacks.md)): `PositionalEncoding`, `Encoder`,
`Decoder` and `Transformer.__init__` check.

## What's missing

Z3 already proves the nonlinear facts the model needs: `h * (d_model // h) = d_model` from
`d_model % h == 0`, that head splitting's `-1` is the sequence length, and `(d+1) // 2 = d // 2` for even
`d`. None of the gaps below is a solver limitation.

### Language (frontend, with IR support where noted)

| # | Gap | Where the spec needs it |
|---|---|---|
| G1 | **Classes and `nn.Module`.** Check methods with `self` typed as the instance. `module(x)` calls `forward`. `super().__init__()`. `__init__` returns `None`. | every class, and `attention`'s `nn.Dropout` parameter |
| G2 | **Instance dims.** A class's constructor ints (and attributes derived from them, e.g. `self.d_k = d_model // h`) are rigid dims shared by its methods' annotations, so `forward(x: "b n d_model")` refers to `__init__`'s `d_model`. Attributes get types from constructor calls (`self.w_q = nn.Linear(d_model, d_model)`), including nested user modules. Stub classes (`nn.Linear`) use the same convention. | every class |
| G3 | **Int parameters in shapes.** `subsequent_mask(size: int) -> "1 size size"` must mean the int's value, or `make_masks` can't broadcast the result against `[b, 1, t]`. Step 9 kept these namespaces apart, following jaxtyping. | `subsequent_mask`, `make_masks` |
| G4 | **`Optional` and `if x is not None`.** Narrowing, and joining the branches' shapes (`mask = mask.unsqueeze(1)` in one branch). | `attention`, `MultiHeadedAttention.forward` |
| G5 | **Loops over a `ModuleList`** of identically configured modules, built by a comprehension over `range`. The loop variable's declared shape is the invariant. | `Encoder`, `Decoder` |
| G6 | **Tuple return types** (`tuple[A, B]`) and unpacking. | `make_masks` |
| G7 | **Asserts as assumptions**, including `%`. The core's constraints have no modulo, but `a % b == 0` can be written `b * (a // b) = a`. | `MultiHeadedAttention.__init__`, `PositionalEncoding` |
| G8 | **Ints used as sizes.** `torch.ones(1, size, size)` needs `size ≥ 0`. Infer it as a precondition that callers prove, rather than rejecting the body. | `subsequent_mask` |
| G9 | **Attributes:** assignment (weight tying checks `[vocab, d_model]` against `[vocab, d_model]`), class-level annotations (`pe: Float[...]`), and `register_buffer`. | `Transformer.__init__`, `PositionalEncoding` |
| G10 | **Slicing:** symbolic bounds (`pe[:, :n]` is `min(n, max_len)` rows) and strided slice assignment (`pe[:, 0::2] = …` needs `(d+1) // 2` columns). | `PositionalEncoding` |

### Core

| # | Gap | Where the spec needs it |
|---|---|---|
| G11 | **`Broadcast` should mean "broadcasts to".** jaxtyping's `*#b` accepts a shape of lower rank with 1s, and rejects a higher rank (checked at run time). The core only checks compatibility, which is weaker than jaxtyping and too weak for `masked_fill` or `x += y`. | `attention` |
| G12 | **Single broadcastable dims `#q`:** a rigid dim constrained to `q ∨ 1`, which `broadcast_pair` already handles. | `attention`, `MultiHeadedAttention.forward` |
| G13 | **`-1` in `view`/`reshape`:** a dim equal to `prod(A) // prod(rest)`, with a divisibility proof. | `MultiHeadedAttention.forward`, `loss` |
| G14 | **`transpose(i, j)` for any literal axes:** a `swap` list function. Stubs cover only `(0, 1)` and `(-2, -1)`. | `MultiHeadedAttention.forward` |

### Stubs

| # | Missing |
|---|---|
| G15 | `x.size(i)` (a `Dim`), `math.sqrt`/`math.log`, `torch.triu`, `masked_fill` (after G11), `&`, `torch.arange(start, end, step)`, `sin`/`cos`, `F.cross_entropy`, and classes for `nn.Linear`, `nn.Dropout`, `nn.LayerNorm`, `nn.Embedding`, `nn.ModuleList`. |

## Decisions

All four were decided as recommended (2026-09-24). Milestone 1 implemented 1 and 3. Checking `loss` extended
3: torch can't infer a `-1` when the other sizes multiply to 0, so the checker also infers `d >= 1` for dims
([13-free-functions.md](13-free-functions.md#inferred-preconditions)).

1. **Int parameters in shapes (G3).** Recommendation: a dim named after an int parameter or constructor
   int is that int's value, as in stubs. Other names keep jaxtyping's meaning. At run time jaxtyping just
   binds such a name per call, so the spec still runs. This reverses step 9's rename-apart rule for ints.
2. **Instance dims (G2).** Recommendation: the constructor-int convention above, for user classes and stub
   classes alike, rather than new annotation syntax. The alternative is class-level annotations on every
   attribute (`self.w_q: Linear["d_model d_model"]`), which isn't jaxtyping.
3. **Ints as sizes (G8).** Recommendation: infer `requires n ≥ 0`. torch raises on negative sizes anyway,
   so this moves a runtime error to the caller.
4. **Broadcast semantics (G11).** Recommendation: match jaxtyping ("broadcasts to").

## Suggested order

Callers only use signatures, so a method can check before the methods it calls do.

| Milestone | Gaps | Targets it unlocks |
|---|---|---|
| 1. Free functions (done) | G3, G6, G8, G13, and G15's functional stubs | `subsequent_mask`, `make_masks`, `loss` |
| 2. Modules (done) | G1, G2, and G15's `nn` classes | `PositionwiseFeedForward`, `Embeddings`, `EncoderLayer.__init__`, `DecoderLayer.__init__`, `Transformer.encode`/`decode`/`forward` (and `MultiHeadedAttention.__init__`) |
| 3. Attention (done) | G4, G7, G11, G12, G14 | `attention`, `MultiHeadedAttention`, and `EncoderLayer.forward`/`DecoderLayer.forward`, whose calls need its `Optional` and `#q` signature |
| 4. Stacks and encodings (done) | G5, G9, G10, and G15's `arange`, `sin`/`cos` | `Encoder`, `Decoder`, `PositionalEncoding`, `Transformer.__init__` |

Milestone 2 decided how modules are typed: they're lowered to their constructor's ints, and methods assume
a class invariant ([14-modules.md](14-modules.md)). Milestone 3 decided how `Optional` is checked: once for
each choice of which `Optional` parameters are `None` ([15-attention.md](15-attention.md)). Milestone 4
made class-level annotations the types of attributes, and a loop's invariant that the locals it
reassigns keep their shapes ([16-stacks.md](16-stacks.md)). After each milestone, add the names that now
check to `PASSING` in `test_goal.py`.
