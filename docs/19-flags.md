# nanoGPT milestone 2: flags

**Gap:** nanoGPT's modules branch on `bool`s whose values aren't known statically (N4 and N5 in
[17-nanogpt-goal.md](17-nanogpt-goal.md)). `LayerNorm`'s `bias` is `None` unless its constructor's flag is
set, and `CausalSelfAttention.__init__` registers its mask buffer only when `hasattr` finds no flash
attention. Milestone 3 decided `if`s on whether a local is `None` ([15-attention.md](15-attention.md)), and
any other `if` was an error. Now 7 of 11 check: `LayerNorm` (`__init__` and `forward`) and
`CausalSelfAttention.__init__` are new. This took a small change to the checker's CLI, and none to the
core.

## Design: a case per value of each flag

Decision 5 of the goal: a method is checked once for each value of each flag it depends on, and the
checker finds the flags from the code. nanoGPT is unchanged.

```python
class LayerNorm(nn.Module):
    def __init__(self, ndim: int, bias: bool):
        super().__init__()
        self.weight = nn.Parameter(torch.ones(ndim))
        self.bias = nn.Parameter(torch.zeros(ndim)) if bias else None

    def forward(self, input: Float[Tensor, "*b ndim"]) -> Float[Tensor, "*b ndim"]:
        return F.layer_norm(input, self.weight.shape, self.weight, self.bias, 1e-5)
```

**What a flag is.** A flag is either of these:

- a `bool` parameter of `__init__`, like `LayerNorm`'s `bias`, or
- an attribute that `__init__` sets once, at its top level, to a flag expression: a flag, `True` or
  `False`, `hasattr(x, "name")`, or `not`/`and`/`or` of those, like `CausalSelfAttention`'s `self.flash`.

A flag expression is evaluated from the flags it's made of, so `self.use_bias = bias` is the flag `bias`
itself. Only `hasattr(...)` is opaque. When it's the attribute's whole value, the attribute is a flag of
its own, named `self.flash`. Setting a flag attribute emits nothing in the IR, since it computes no shape.

**Finding the cases.** A body is translated with a *case*, the values of the flags tested so far, starting
with none. When it tests a flag the case doesn't give, it's translated again with the flag `True` and
`False`. The test may be in the body (`if not self.flash:`, `x if bias else y`), or in the value of an
attribute the body reads. `LayerNorm.forward` reads `self.bias`, whose value tests `bias`, so it has two
cases. A flag tested only under another is split only there. `CausalSelfAttention.__init__` has two
cases, and a module that tested `self.use_shift` inside `if not self.fast:` would have three:
`[self.fast=True]`, `[self.fast=False,shift=True]` and `[self.fast=False,shift=False]`.

**A case is a checker function.** It's named after its flags, as an `Optional` variant is after its
`None`s: `LayerNorm.forward[bias=False]`, or `f[mask=None,bias=False]` with both. In each case:

- `if flag:`, `x if flag else y`, and `not`/`and`/`or` of flags are decided, and only the branch taken is
  translated.
- An attribute whose value is `None` in the case is `None`: in `[bias=False]`, `self.bias` is `None`. So
  it selects `layer_norm`'s case without a bias, as a `None` local selects an `Optional` parameter's. It
  can also be tested (`if self.bias is not None:`), and using it as a value is an error:

  ```
  flag_none.py:15: in Affine.forward[bias=False]: `self.bias` is None here
  ```

- `__init__` assigns such an attribute `None` (`["Tuple", []]`) in the IR.

**One signature.** Unlike an `Optional` parameter, which a caller passes or doesn't, a flag is usually
beyond a caller's control. nanoGPT passes `bias=config.bias`, and nothing picks `self.flash`. So the cases
aren't separate signatures: a function that tests flags is still one function to its callers. The IR has
a function under the plain name with the signature and no body, which calls and `instances` refer to, and
each case is a function with a copy of that signature and a `group` naming it:

```
LayerNorm.__init__               (ndim: int, bias: int) -> None     no body
LayerNorm.__init__[bias=True]    group LayerNorm.__init__
LayerNorm.__init__[bias=False]   group LayerNorm.__init__
```

Two things could make a case's signature differ, and both are merged:

- **Inferred requires.** The checker's CLI keeps inferred requires per group, not per function, so what
  any case infers is a requires of every case and of the function callers see. In
  `CausalSelfAttention.__init__`, only the slow case builds `torch.ones(block_size, block_size)`, and the
  constructor requires `block_size >= 0` for both. `Block.__init__` infers it in turn. The CLI's
  constructors-first phase includes the cases of a constructor, and several cases inferring the same fact
  in one round add it once.
- **Asserts in `__init__`.** An assert about the instance dims is an ensures of the constructor. Only the
  ensures every case has are kept, since an assert that only some cases reach isn't a fact about every
  instance.

A function passes if every case of every variant checks. Errors are reported once per line and message
across the cases, and inferred requires are reported once, for the function.

The IR for `CausalSelfAttention.__init__[self.flash=False]` ends with the buffer, which the other case
doesn't have. `self.flash = hasattr(...)` and the `print`, which prints only a string, emit nothing:

```
Assume n_head * (n_embd // n_head) = n_embd
self.c_attn = torch.nn.Linear.__init__(n_embd, 3 * n_embd, bias)
...
self.dropout = Scalar
self.bias = torch.tril(torch.ones(block_size, block_size)).view(1, 1, block_size, block_size)
```

### Alternatives

- **`@overload`s on `Literal[True]`/`Literal[False]`** (the goal's decision 5). That's exact, but three
  signatures for one constructor, and it changes nanoGPT.
- **A maybe-`None` value**, which only an `Optional` parameter accepts. It would check `LayerNorm`, but it
  can't relate `self.flash` to the buffer registered under `if not self.flash:`, which milestone 3 needs.
- **Cases as separate signatures**, like `Optional` variants. Callers can't pick one, since `bias=
  config.bias` isn't a literal. They'd have to call every case, and join the results.
- **A flag parameter in the case's IR.** A case could keep `bias` as a parameter and assume
  `bias == 0`. Nothing needs its value, since the frontend decides every test on it.

## `Optional` in stubs

`F.layer_norm`'s `weight` and `bias` are `Optional`, and so will `scaled_dot_product_attention`'s
`attn_mask` be in milestone 3. Stubs may now have `Optional` parameters, as user functions do:

```python
# normalized_shape is one dim, the last: torch allows several
def layer_norm(
    input: Float[Tensor, "*B n"],
    normalized_shape: Shape["n"],
    weight: Optional[Float[Tensor, "n"]] = None,
    bias: Optional[Float[Tensor, "n"]] = None,
    eps: float = 1e-5,
) -> Float[Tensor, "*B n"]: ...
```

Each overload has a signature for each choice of which `Optional` parameters are `None`. A call with
`None` goes to that case, named like `torch.nn.functional.layer_norm[bias=None]`, and the `None` isn't
passed.

## `x.shape`

`LayerNorm.forward` passes `self.weight.shape` as `normalized_shape`. In the core, a shape is an array
value, so `Shape["*S"]` parameters take `Dimensions`. `x.shape` could be one too, but a local holding it
would then be a tensor to the core: `x.shape + 1` would broadcast. So `Tensor.shape` is a stub property
returning `Shape["*A"]`, and the frontend only allows `x.shape` where a shape is expected: as an argument
to a `Shape[...]` parameter, or as the only argument to `*size: Shape[...]`, so `torch.zeros(x.shape)`
works. Elsewhere it's an error, as before.

## Soundness notes

- **Every instance is in a case.** Each flag has a value when a method runs: `__init__`'s argument, or the
  attribute's one value. Both values are checked, so whatever it is, a checked case covered it. Flags
  that can't differ, like two `hasattr`s of the same name, are still split independently, which checks
  cases that can't happen. That can only reject a program.
- **Flags don't change.** `__init__` can't reassign a `bool` parameter, and a flag attribute is assigned
  once, at the top level of `__init__`, as any attribute a method reads must be. Code outside the class
  could still assign `model.flash`, which the checker assumes it doesn't, as for any attribute.
- **`None` attributes.** Whether an attribute is `None` is decided from its one assignment, so it's the
  value a method sees. A call's result is never `None`, as before.
- **One signature.** Callers prove the requires every case inferred, and assume only the ensures every
  case proved. Each case is checked against that signature.
- **`print`** has no effect on shapes. Its arguments are still translated, so a call in one is checked.
- **`hasattr`** is only a flag's source, not a value. Its object isn't evaluated.
- **`x.shape`** is only a shape where the stub expects one, so a shape is never mistaken for a tensor.

## Other changes

- **Stubs:** `nn.Parameter` (a function of its data), `F.layer_norm`, and the property `Tensor.shape`.
- **Tests on attributes:** `self.x is None` is decided, for an attribute `__init__` assigns, in every
  case. It isn't only for flags.
- **Diagnostics:** passing a `None` attribute or conditional expression where a value is expected is
  `` `self.bias` is None here ``, as for a `None` local, rather than a binding error per overload.
- **nanoGPT:** `Block.__init__` now infers `block_size >= 0`, since `CausalSelfAttention.__init__`
  requires it.

## Tests

- `frontend/tests/test_translate.py`: `Flags` checks the case names and groups, the shared signature with
  no body, `None` attributes in each case (selecting `layer_norm`'s case, and `self.bias is None`), flag
  attributes (`hasattr`, one that's another flag, a flag split only under another, setting one emitting
  nothing, `print`), ensures common to the cases, errors reported once across cases, and the errors: a
  reassigned flag, a test that isn't on a flag, another module's flag, an attribute assigned twice, more
  than 4 flags, and `print(*args)`. `StubOptionals` checks a stub's variants and calls selecting them, and
  `Shapes` checks `torch.zeros(x.shape)` and `x.reshape(x.shape)`.
- `examples/pass/flags.py`: nanoGPT's `LayerNorm`, a scale with an optional shift whose flag is another's,
  a module that builds a mask only without the fast path, and a block passing its flag on. It also runs
  under jaxtyping's runtime checker, for both values of each flag. `test_examples.py` pins that the slow
  path's `max_len >= 0` is required of the constructor, and of `Block`'s in turn, and that each is
  reported once. `examples/fail/flag_none.py` adds a bias that's `None` without the flag, and
  `examples/fail/flag_buffer.py` builds the slow path's mask with the wrong size.
- `frontend/tests/test_gpt_goal.py`: `PASSING` has 7 methods.

## Limitations

- **Attributes assigned in a branch can't be read yet.** `CausalSelfAttention`'s `bias` buffer is
  registered under `if not self.flash:`, so `forward` can't read it. Milestone 3 (N4 in bodies) makes an
  attribute's value per case, so the `[self.flash=False]` case of `forward` has the buffer, and reading
  it in the other is an `AttributeError`.
- **Only `self`'s flags.** A nested module's flag (`if self.ln.use_bias:`) is an error, and so is an
  attribute of a nested module whose value tests one.
- **Not flags:** `bool` parameters of methods and free functions, a config's `bool` fields
  (`config.bias`), and `nn.Module.training`, which milestone 3 needs for `self.dropout if self.training
  else 0`. `hasattr` is only supported as a flag attribute's value.
- **At most 4 flags** per function, so 16 cases, for each variant of its `Optional` parameters.
- **`layer_norm`'s `normalized_shape` is one dim.** torch allows a tuple of trailing dims.
- `x.shape[i]` and `x.shape` as a value are still errors. `x.size()` with no argument is milestone 3
  (N2).
