# Goal: check nanoGPT

**Target:** [`examples/goal/gpt.py`](../examples/goal/gpt.py), Karpathy's
[nanoGPT](https://github.com/karpathy/nanoGPT) `model.py` (commit `f08abb4`) with jaxtyping annotations
added and no other changes. It's GPT-2: pre-norm blocks with causal self-attention (flash, or manual
with a mask buffer), a GELU MLP, learned position embeddings, a `GPTConfig` dataclass for the sizes,
weight tying, and sampling with `generate`.

Left out are the parts that don't compute with the model's tensors: `from_pretrained` (loads Hugging
Face weights), `configure_optimizers`, `estimate_mfu`, `get_num_params`, `_init_weights`, and the lines of
`GPT.__init__` that call them. Also left out is `crop_block_size`, which does model surgery: it assigns
`config.block_size` on a live model. That changes an instance dim after construction, and the module
design assumes instance dims are fixed ([14-modules.md](14-modules.md)). Checking it would need a model's
type to change, which is out of scope.

The spec is correct code. `examples/goal/run_gpt.py` trains a step and generates past `block_size` at two
sizes (one with GPT-2 small's widths), through both attention paths, under jaxtyping's runtime checker. It
also checks that the runtime checker catches targets of the wrong length.

```sh
uvx --python 3.12 --with torch --with numpy --with jaxtyping --with beartype \
    python examples/goal/run_gpt.py
```

**The spec** is `frontend/tests/test_gpt_goal.py`:

- `test_goal` checks that all 11 annotated methods check. It's an expected failure until milestone 5.
- `test_progress` is a ratchet over `PASSING`. It fails if a method stops checking, or if one starts
  checking and isn't recorded.

**Progress:** 4 of 11. Milestone 1 is done ([18-configs.md](18-configs.md)): `MLP` and `Block` check.

## What's missing

The Transformer goal ([12-transformer-goal.md](12-transformer-goal.md)) already covers much of the model:
modules with instance dims, head splitting with `view` and `transpose`, `Optional` parameters checked per
case, a loop over an `nn.ModuleList`, weight tying, and a mask buffer sliced by a symbolic bound. The
gaps below are what nanoGPT adds.

### Language (frontend, with IR support where noted)

| # | Gap | Where the spec needs it |
|---|---|---|
| N1 | **Config objects.** `__init__(self, config: GPTConfig)` takes a `@dataclass`, not ints. Its `int` fields are the instance dims, named by field, so `forward`'s `"b t n_embd"` is `config.n_embd`. Passing it on (`Block(config)`), a `bool` field as a flag (`bias=config.bias`), and storing it (`self.config = config`, read as `self.config.block_size`). The annotation may be a string forward reference (`"GPTConfig"`). | every class but `LayerNorm` |
| N2 | **`x.size()` with no argument:** a tuple of dims, unpacked as `B, T, C = x.size()`. | `CausalSelfAttention.forward`, `GPT.forward` |
| N3 | **`split` into a known number of pieces:** `q, k, v = self.c_attn(x).split(self.n_embd, dim=2)`. The number of pieces is the number of names unpacked, and the checker must prove there are that many: `(k-1)*s < x[dim] <= k*s`. | `CausalSelfAttention.forward` |
| N4 | **`if` on a runtime flag.** `if self.flash:` isn't known statically, unlike milestone 3's `is None` tests, so both branches are checked, and a local assigned in both (`y`) must get the same shape. The same goes for `a if self.training else 0`. An attribute may be assigned in a branch (`self.register_buffer("bias", ...)` in `if not self.flash:`), and without a class-level declaration, its value is its type. Reading it where it wasn't assigned is an `AttributeError`, not a shape error. Also `hasattr`, `print` and `nn.Module.training`. | `CausalSelfAttention` |
| N5 | **Optional attributes on a `bool`:** `self.bias = nn.Parameter(torch.zeros(ndim)) if bias else None` is passed to `F.layer_norm`'s optional `bias`. Whether it's `None` depends on a constructor `bool`, which isn't an instance dim, so milestone 3's per-case split doesn't decide it. | `LayerNorm` |
| N6 | **`nn.ModuleDict(dict(wte=..., h=nn.ModuleList(...), ...))`:** attributes of a dict of modules (`self.transformer.wte(idx)`, `for block in self.transformer.h`), and weight tying through one (`self.transformer.wte.weight = self.lm_head.weight`). | `GPT` |
| N7 | **Int and list indexing:** `logits[:, -1, :]` drops a dim, and `x[:, [-1], :]` keeps it with size 1. Both need the dim to be at least 1. | `GPT.forward`, `generate` |
| N8 | **`Optional` in return types:** `forward` returns `(logits, None)` when `targets` is `None`. Callers unpack it (`logits, _ = self(idx_cond)`). | `GPT.forward`, `generate` |
| N9 | **Values that aren't shapes:** `device = idx.device` passed as `device=`, `dtype=torch.long`, `float("-inf")`, and an f-string as an assert's message. | `GPT.forward`, `CausalSelfAttention.forward` |
| N10 | **Loops whose shapes change.** `generate` runs `idx = torch.cat((idx, idx_next), dim=1)` for each of `range(max_new_tokens)`, so `idx` isn't a loop invariant: its length grows with the iteration count. `idx_cond` is `idx` or its last `block_size` columns, chosen at run time. Also `@torch.no_grad()`, `min` on ints, `Optional[int]`, and assignment through a boolean mask (`logits[logits < v[:, [-1]]] = ...`). | `generate` |

### Core

| # | Gap | Where the spec needs it |
|---|---|---|
| N11 | **`#t` in return types:** an existential dim that's `t` or 1, which jaxtyping checks at run time. `forward`'s logits are `[b, t, vocab_size]` with targets and `[b, 1, vocab_size]` without. | `GPT.forward` |
| N12 | **Inferred relational preconditions.** The manual path's `self.bias[:, :, :T, :T]` has `min(T, block_size)` rows, and broadcasting it against `[B, nh, T, T]` needs `T <= block_size`. nanoGPT only asserts that in `GPT.forward`. Inferred preconditions (milestone 1 of the Transformer) are sizes only: `n >= 0` and `d >= 1`. This adds a slice's stop being within its dim as a candidate. | `CausalSelfAttention.forward`, and `Block.forward` passes it on |
| N13 | **Loop invariants over an iteration count:** after `i` iterations, `idx` is `[b, t + i]`, so after the loop it's `[b, t + max_new_tokens]`. The loop's variable has to be in scope for the invariant, and the loop's end is `max_new_tokens`. | `generate` |

### Stubs

| # | Missing |
|---|---|
| N14 | `nn.Parameter`, `nn.GELU`, `nn.ModuleDict`, `x.shape`, `F.layer_norm` (a shape, and optional `weight`/`bias`), `Tensor.split`, `F.scaled_dot_product_attention` (`attn_mask=None`, `dropout_p`, `is_causal`), `torch.arange`'s `dtype`/`device`, `F.cross_entropy`'s `ignore_index`, `torch.topk`, `torch.multinomial`, `torch.cat`, and the builtins `hasattr`, `print`, `float` and `min`. |

## Decisions

All four were decided as recommended (2026-09-26):

1. **Config objects (N1).** A `@dataclass` parameter's `int` fields are instance dims, named by field,
   by the same rule as constructor ints. `self.config = config` stores it, and `self.config.block_size`
   is the dim. There's no new syntax, and nanoGPT is unchanged. The alternative was rewriting the goal
   so each module takes its ints, which is no longer nanoGPT.
2. **`forward`'s logits (N11).** Annotate them `"b #t vocab_size"`. It's plain jaxtyping and runs. The
   alternatives were an `@overload` per case of `targets`, which is exact but three signatures for one
   method, and `"b _ vocab_size"`, which leaves the dim unchecked.
3. **`T <= block_size` (N12).** Infer it, as a new kind of inferred precondition. `CausalSelfAttention.
   forward` infers it, `Block.forward` passes it on, and `GPT.forward` proves it from its assert. The
   alternative was an assert in the manual branch, as the Transformer has for `pe`, which changes nanoGPT.
4. **`generate` (N10, N13).** It's in the goal, as the last milestone. It's annotated `-> Int[Tensor, "b
   n"]`, because jaxtyping can't evaluate `"b t+max_new_tokens"` at run time: `max_new_tokens` is an
   argument, not a dim (`AnnotationError: Cannot process symbolic axis`). The checker may still prove the
   length.

## Suggested order

Callers only use signatures, so a method can check before the methods it calls do. Each milestone gets a
doc, as the Transformer's did.

| Milestone | Gaps | Targets it unlocks |
|---|---|---|
| 1. Configs (done) | N1, and `nn.GELU` | `MLP`, `Block` (4) |
| 2. Flags | N4 in `__init__`, N5, and `nn.Parameter`, `x.shape`, `F.layer_norm`, `hasattr`, `print` | `LayerNorm`, `CausalSelfAttention.__init__` (3) |
| 3. Causal attention | N2, N3, N4 in bodies, N9's `float`, N12, and `F.scaled_dot_product_attention`, `Tensor.split` | `CausalSelfAttention.forward` (1) |
| 4. The model | N6, N7, N8, N9, N11, and `nn.ModuleDict`, `arange`'s keywords, `cross_entropy`'s `ignore_index` | `GPT.__init__`, `GPT.forward` (2) |
| 5. Generation | N10, N13, and `topk`, `multinomial`, `cat`, `min` | `GPT.generate` (1) |

Milestone 1 made a config's `int` fields instance dims, and its flags parameters of `__init__` only
([18-configs.md](18-configs.md)). Milestone 2 decides how a value whose `None`-ness depends on a `bool` is checked (N5): cases per flag, like
milestone 3 of the Transformer, or a maybe-`None` value that only an `Optional` parameter accepts.
Milestone 5 decides how a loop's invariant names the iteration count (N13). After each milestone, add the
names that now check to `PASSING` in `test_gpt_goal.py`.

**Stretch goal: Llama** (Meta's reference `model.py`, or LitGPT's), unchanged from
[09-future-work.md](09-future-work.md#next-goal-nanogpt).
