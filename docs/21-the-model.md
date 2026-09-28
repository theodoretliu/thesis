# nanoGPT milestone 4: the model

**Gap:** `GPT` ([17-nanogpt-goal.md](17-nanogpt-goal.md)) keeps its modules in an `nn.ModuleDict` and ties
the output projection's weight through it (N6). Its `forward` has four other gaps. It takes the last
position with `x[:, [-1], :]` (N7). It returns `(logits, None)` without targets (N8). It makes positions
with `torch.arange(0, t, dtype=torch.long, device=device)` (N9). And its logits are annotated
`"b #t vocab_size"` (N11). Now 10 of 11 check: `GPT.__init__` and `GPT.forward` are new, and only
`generate` (milestone 5) is left. This took one change to the core's types, one to its terms, three
frontend features and stubs.

```
gpt.py:150: note: GPT.__init__ requires vocab_size >= 0, n_embd >= 0, block_size >= 0, n_head >= 1 (inferred from its body)
gpt.py:174: note: GPT.forward requires vocab_size >= 1, n_embd >= 1 (inferred from its body)
gpt.py:174: note: GPT.forward[targets=None] requires t >= 1, n_embd >= 1 (inferred from its body)
```

`GPT.forward` proves the blocks' `t <= block_size` from its assert, as decision 3 of the goal planned.
With targets, `logits.view(-1, logits.size(-1))` infers `vocab_size >= 1`, since torch can't infer a `-1`
when the other sizes multiply to 0. Without targets, `x[:, [-1], :]` infers `t >= 1`.

## `nn.ModuleDict`: a class of its entries

```python
self.transformer = nn.ModuleDict(
    dict(
        wte=nn.Embedding(config.vocab_size, config.n_embd),
        wpe=nn.Embedding(config.block_size, config.n_embd),
        drop=nn.Dropout(config.dropout),
        h=nn.ModuleList([Block(config) for _ in range(config.n_layer)]),
        ln_f=LayerNorm(config.n_embd, bias=config.bias),
    )
)
...
self.transformer.wte.weight = self.lm_head.weight
...
tok_emb = self.transformer.wte(idx)
for block in self.transformer.h:
    x = block(x)
```

Everything about attributes goes through two steps: find the instance an expression is (`self`, or
`self.f` built by a constructor call), then look up a field of its class. So a `ModuleDict` attribute is
an instance of a class of its own (`dict_class`). Its fields are the dict's entries, and its instance
dims, configs and flags are those of the class that assigns it. Entries' arguments like
`config.vocab_size` are over that class's `__init__` names, so building `self.transformer.wte` is building
an attribute of `GPT`: the same instance dims, substituted the same way. The class is named
`GPT.transformer`, and nothing else changes. Loops over `self.transformer.h`, weight tying through
`self.transformer.wte`, and nested dicts work because they only use those two steps.

Like `nn.ModuleList`, it's recognized by name (`torch.nn.ModuleDict`), with no stub. The forms are
`nn.ModuleDict(dict(name=..., ...))` and `nn.ModuleDict({"name": ..., ...})`, with names for keys, so
that entries are attributes. Each entry must be a module: a constructor call or an `nn.ModuleList`. In
`__init__`, the assignment builds every entry, as one `Let` of a tuple of their constructor calls. So
`GPT.__init__` infers what `nn.Embedding` and `Block` require.

The dict itself isn't a module that can be called, and it can't be used as a value:

```
in GPT.forward: `GPT.transformer` is an nn.ModuleDict, so it can't be called; its entries can
```

An entry's value is evaluated with no `self`, so an entry built from another attribute
(`h=nn.ModuleList([self.block ...])`) is an error. Nothing needs one.

### Alternatives

- **Dotted fields**, `transformer.wte` as a field of `GPT`. Every place that looks up `self.f` would
  need to handle a path, where a class of the entries needs no change to them.
- **A stub class with `__getattr__`.** Stubs have no way to say an attribute's type depends on the
  constructor's arguments, which is what each entry is.

## Values that aren't shapes

```python
device = idx.device
pos = torch.arange(0, t, dtype=torch.long, device=device)
```

A `torch.device` or a `torch.dtype` doesn't affect shapes, so it has no IR at all. The stubs declare
them as kinds of values:

```python
class dtype: ...
class device: ...

long: dtype

class Tensor:
    device: device
    dtype: dtype

@overload
def arange(start: int, end: int, step: int = 1, *, dtype: Optional[dtype] = None, device: Optional[device] = None) -> ...
```

- **A kind** is a stub class with nothing in it. A module-level annotation declares a constant of it
  (`torch.long`). An annotation in another class, like `Tensor`, declares an attribute of arrays that is one
  (`x.device`).
- **A parameter annotated with a kind** takes such a value, or `None`, and is bound but not passed
  (`OPAQUE`, like `NONE`). Its `Optional` doesn't make a case, since nothing about it is checked.
- **A local** can hold one: `device = idx.device` binds `device` with no IR. It can be passed where its
  kind is expected, and nothing else:

  ```
  `d` is a `torch.device`, which isn't a shape: it can only be passed where one is expected, or assigned to a name
  ```

`x.device` is only supported on a local variable, so no call's preconditions are skipped. The creation
functions (`zeros`, `ones`, `empty`, `rand`, `randn`, `arange`) take `dtype` and `device`.

### Alternatives

- **An opaque type in the core.** It would check nothing the frontend doesn't, and every function
  taking a device would have one more parameter.
- **Dropping unknown keyword arguments.** That would hide real mistakes, like a misspelled `dim=`.

## Indexing with ints and lists

`x[:, [-1], :]` is the last position, with its dim kept at size 1, and milestone 5's `logits[:, -1, :]`
drops the dim. The core's `Slice` term now has three kinds of item:

- `Range (start, stop, step)`: a slice, as before.
- `Point i`: an int, which drops the dim.
- `Points [i; ...]`: a list of ints, which keeps the dim with as many entries.

An int must be in `[-n, n)`, or torch raises an `IndexError`. That's a size obligation, so it may be
inferred: `-1` needs `n >= 1`, which is `t >= 1` for `GPT.forward[targets=None]`. `x[:, [0, 1]]` needs
`n >= 2`, which isn't a candidate, so it must be provable.

```
index_range.py:9: in second, `return pair[:, 2]`: index 2 may be out of range for a dim of size 2
```

torch's advanced indexing moves dims when there's more than one list, or a list and an int
(`x[[0, 1], :, [0, 1]]` is `[2, n1]`), so only one list is supported, and not with ints. An int's value
must be known well enough to be proved in range. An int of unknown value, a mask, `None` and `...` are
errors.

### Alternatives

- **Rewriting `x[:, [-1], :]` as `x[:, -1:, :]`**, a slice with the same shape. It wouldn't need
  `t >= 1`: a slice of an empty dim is empty. That's weaker than what torch does, since `x[:, [-1]]` of an
  empty dim raises.
- **Encoding an index as a slice `i:i+1` plus a squeeze.** `-1:0` is empty, so negative indices would
  need special cases, and the error would be about a squeeze.

## `Optional` in return types

```python
def forward(self, idx, targets=None) -> tuple[Float[Tensor, "b #t vocab_size"], Optional[Float[Tensor, ""]]]:
    ...
    return logits, loss   # loss is None without targets
```

The frontend already checks `forward` once with `targets` and once without (`GPT.forward[targets=None]`),
but both have the one annotation. So the core has a return type `TypeOptional t`, for return types
only:

- **Returning** `None` (the empty tuple) or a `t` checks. An item of a returned tuple that's `None`,
  where the return type's item is `Optional`, is the empty tuple. Elsewhere, `None` is still an error.
- **A caller** gets `Maybe v`, which is `None` or `v`. It can unpack it (`logits, _ = self(idx_cond)`)
  or return it as an `Optional`, and nothing else, since which one it is isn't known:

  ```
  in Trainer.scaled_loss, `return loss * 2`: operator.mul: ... parameter a (array[*A], given None or array of shape []): expected an array
  ```

- A parameter can't have the type. The frontend splits `Optional` parameters into cases instead.

### Alternatives

- **A return type per case**, where `forward[targets=None]` returns `None` for the loss. The annotation
  doesn't say that, and callers only have the annotation. The frontend would need to infer it from the
  body, or the user to write `@overload`s, which the goal rules out for `#t` too (decision 2).
- **Testing the result**, `if loss is not None:` on a returned value. That's a test at run time, which
  isn't supported. Nothing needs it.

## `#t` in return types

Decision 2 of the goal annotates the logits `"b #t vocab_size"`: `t` or 1. That was rejected everywhere
a return type was checked. Now `#n` may be in a return type if a parameter binds `n`, in the frontend and
in `check_signature`. It can't bind `n` itself, since `n` would then be whatever the body returned.

- **Returning:** the dim must be `n` or 1, which matching `BroadcastDim` already checked for parameters.

  ```
  last_position.py:20: in LM.forward[targets=None], `return self.head(x[:, -1, :]), None`: return value[0] (array[b, #t, vocab], given array of shape [b, vocab]): expected t or 1, got vocab
  ```

- **A caller** gets a fresh dim `d`, with `d = n ∨ d = 1` assumed, as a parameter's `#n` is inside a
  body. So `generate` (milestone 5) can prove `logits[:, -1, :]` is in range once `t >= 1`: both cases are
  at least 1. `examples/pass/model.py` does this in `next_token_logits`.

### Alternatives

- **A fresh dim with `1 <= d <= n`.** It's weaker, and contradictory for `n = 0`, where `#n` allows 0 or 1.

## Soundness notes

- **`nn.ModuleDict`:** the entries are built in `__init__` and checked there, like attributes, and each
  is read by the same field lookup. The entries' class shares its parent's instance dims, so the
  substitution that maps an attribute's constructor arguments to a method's dims is the same.
- **Values that aren't shapes** have no effect on shapes in torch. They're bound and dropped, and a
  receiver of `.device` must be a local, so no expression with preconditions is dropped.
- **Indexing:** an int or list must be provably in range, or inferred as a size fact, which callers
  prove. The dims are torch's for one list and no ints, or for ints and slices, and anything else is
  rejected.
- **`Optional` returns:** a caller can only unpack or return a `Maybe`, so it never uses a value that may
  be `None` as a tensor.
- **`#n` returns:** the body proves the dim is `n` or 1, and callers assume only that.

## Other changes

- **Stubs:** `torch.dtype`, `torch.device`, the dtypes as constants, `Tensor.device` and
  `Tensor.dtype`, and `dtype`/`device` keywords on `zeros`, `ones`, `empty`, `rand`, `randn` and
  `arange`. `cross_entropy`'s `ignore_index` was already stubbed.
- **Frontend:** `return None` in a function whose return type is `Optional`.
- **IR:** `["Optional", typ]`, and slice items `["Index", term]` and `["List", [term]]`, next to
  `[start, stop, step]` ([frontend/README.md](../frontend/README.md#the-ir)).

## Tests

- `checker/test/test_bodies.ml`: `x[:, -1]` with `n >= 1` given, rejected, and inferred; `x[:, [-1], :]`;
  `x[:, [0, 1]]` needing `n >= 2`, which isn't inferred; an int parameter as an index; two lists, a list
  and an int, and too many indices. `#n` in a return type accepting `n` and 1 and nothing else, and a
  caller's dim being `n` or 1, so at least 1 only when `n` is. An `Optional` in a return type accepting
  `None` and its type and nothing else, a caller unpacking it but not using it as an array, and
  returning it as an `Optional`. `#n` in a signature's return type needing `n` bound, which replaces the
  test that it was rejected, and `Optional` parameters rejected.
- `frontend/tests/test_translate.py`: `Indexing` (the IR of int and list items), `ModuleDicts` (the
  entries built in `__init__`, from `dict(...)` and a dict literal, weight tying and a loop through
  entries, and the errors: calling the dict, using it as a value, a missing entry, other forms, and an
  entry that isn't a module), `OptionalReturns` (the return type in both cases, `None` where it's
  `Optional`, and the errors), and `Values` (bound and not passed, the stubs, and the errors).
- `examples/pass/model.py`: a small language model with a `ModuleDict`, weight tying through it,
  positions on the input's device, and nanoGPT's `forward`, with callers that take the last position
  and the first state. `test_examples.py` pins what it infers. It also runs under jaxtyping's runtime
  checker, at several lengths and two configs. There, `t = 0` raises an `IndexError` where the checker
  infers `t >= 1`, and a sequence past `block_size` fails the assert.
- `examples/fail/last_position.py` returns `x[:, -1, :]`'s logits where the length is `#t`,
  `optional_loss.py` uses the loss without targets, `index_range.py` indexes a pair at 2, and
  `dict_tie.py` ties a weight of the wrong shape through a `ModuleDict`. The first three fail at run time
  too. The last runs until the embedding is used, as `tie_shape.py` does.
- `frontend/tests/test_gpt_goal.py`: `PASSING` has 10 methods.

## Limitations

- **`nn.ModuleDict`:** keys must be names, and entries modules built in the dict. Indexing it
  (`self.transformer["wte"]`), iterating it, and `.items()` aren't supported.
- **Values that aren't shapes:** only the kinds the stubs declare, `x.device` only on a local, and no
  `x.to(device)` or user functions taking one.
- **Indexing:** one list, and not with ints. No masks (`logits[logits < v]`, which milestone 5 assigns
  through), `None` or `...`.
- **`Optional` returns** can't be tested (`if loss is not None:`) or used as a value, only unpacked or
  returned.
- **`#n` in a return type** needs `n` bound by a parameter, and `*#A` in a return type isn't supported.
