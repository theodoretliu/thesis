# nanoGPT milestone 1: configs

**Gap:** every nanoGPT module but `LayerNorm` is built from a `GPTConfig` dataclass, not from ints (N1 in
[17-nanogpt-goal.md](17-nanogpt-goal.md)). `MLP.forward(x: Float[Tensor, "b t n_embd"])` refers to
`config.n_embd`, but the frontend only knew instance dims from `__init__`'s `int` parameters, and it
rejected `config: GPTConfig` as an unsupported annotation. Now 4 of 11 check: `MLP` and `Block`
(`__init__` and `forward` of each). This needed only the frontend and the `nn.GELU` stub, with no change to
the core.

## Design: a config is its fields

Decision 1 of the goal: a config's `int` fields are instance dims, named by field, by the same rule as
constructor ints. There's no new syntax:

```python
class MLP(nn.Module):
    def __init__(self, config: "GPTConfig"):
        super().__init__()
        self.c_fc = nn.Linear(config.n_embd, 4 * config.n_embd, bias=config.bias)
        ...

    def forward(self, x: Float[Tensor, "b t n_embd"]) -> Float[Tensor, "b t n_embd"]: ...


@dataclass
class GPTConfig:
    block_size: int = 1024
    ...
    n_embd: int = 768
    dropout: float = 0.0
    bias: bool = True
```

The frontend lowers a config away, as it does modules ([14-modules.md](14-modules.md)):

- **A config class** is a top-level class decorated with `@dataclass` (or `@dataclass(...)`), known by
  its annotated fields. `int` fields are dims, `bool` fields are flags, and `float` fields don't affect
  shapes. Any other field can't be used. Config classes are collected before anything else, since
  nanoGPT defines `GPTConfig` after the modules that take one.
- **A config parameter** is annotated with a config class. The annotation may be a string, since a
  forward reference like `"GPTConfig"` is needed when the class is defined later. In the IR, the parameter
  is its `int` and `bool` fields, as int parameters named by field, in the class's order.
  `MLP.__init__` takes `block_size, vocab_size, n_layer, n_head, n_embd, bias`. A field can't have the
  name of another parameter.
- **Instance dims:** a module whose `__init__` takes a config has the config's `int` fields as instance
  dims, after or between its own `int` parameters, in parameter order. So `MLP.forward` takes
  `block_size, vocab_size, n_layer, n_head, n_embd, x`, and its annotation's `n_embd` is the instance
  dim. The flags aren't instance dims, as a `bool` parameter isn't.
- **Reading a field:** `config.n_embd` is the term for that field, and `config.dropout` is a float
  (`Scalar`). In a method, a module's config has only its instance dims. Reading a flag there is an
  error, although `__init__` can use it (`bias=config.bias`). Methods only compute constructor
  arguments that are instance dims, so building `self.c_fc` again in `forward` never needs `bias`.
- **Passing it on:** `Block(config)` and `MLP(config)` pass the config's fields. Each instance dim
  records where it came from (a parameter, or a parameter's field), so `self.attn(x)` in `Block.forward`
  computes `CausalSelfAttention`'s dims from `Block`'s own:
  `CausalSelfAttention.forward(block_size, vocab_size, n_layer, n_head, n_embd, ...)`.
- **Storing it:** `self.config = config` makes `self.config` that config, so `self.config.block_size` in a
  method is the instance dim. Using a config as a value (`return self.config`) is an error, and so is
  building one (`GPTConfig(...)`) in checked code.
- **Asserts:** `assert config.n_embd % config.n_head == 0` states its fact about the fields' IR names.
  A fact about the instance dims is part of the class invariant ([15-attention.md](15-attention.md)), and
  milestone 3's `view(B, T, n_head, C // n_head)` needs `CausalSelfAttention`'s. A field used to go into a
  let like `(config.n_embd)`, which the invariant would drop.

A local in `__init__` or a method may have a field's name (`n_embd = config.n_embd`). It gets another IR
name (`n_embd'`), as a local named after an instance dim did in methods. `__init__` still can't reassign
one of its own `int` parameters.

The block's IR:

```
Block.__init__(block_size, vocab_size, n_layer, n_head, n_embd, bias: int):
  self.ln_1 = LayerNorm.__init__(n_embd, bias)
  self.attn = CausalSelfAttention.__init__(block_size, vocab_size, n_layer, n_head, n_embd, bias)
  ...
Block.forward(block_size, vocab_size, n_layer, n_head, n_embd: int, x: [b, t, n_embd]):
  x = x + CausalSelfAttention.forward(block_size, ..., n_embd, LayerNorm.forward(n_embd, x))
  x = x + MLP.forward(block_size, ..., n_embd, LayerNorm.forward(n_embd, x))
```

`MLP.__init__` and `Block.__init__` infer `n_embd >= 0` from `nn.Linear` and `LayerNorm`, which becomes
part of the class invariant, so neither `forward` infers anything.

### Alternatives

- **Rewrite the goal to take ints.** No frontend change, but it's no longer nanoGPT, and config
  dataclasses are how most model code is written.
- **Names like `config.n_embd` in annotations.** jaxtyping can't evaluate them, and they'd be new syntax.
- **Every field as an instance dim, including flags.** It would let methods read `self.config.bias`, and
  milestone 2 may want flags in instances to decide `LayerNorm`'s optional bias (N5). Nothing needs it
  yet, and it would make a config's flags parameters of every method.

## Soundness notes

- A dataclass is mutable. The checker assumes a config isn't changed after a module is built from it,
  since it's stored as instance dims that are fixed. Checked code can't assign a field (`config.n_embd =
  ...` is an unsupported assignment), but unchecked code can. `crop_block_size` does, which is why it's
  left out of the goal.
- A field's annotation is trusted: `n_embd: int` is an int, as jaxtyping and dataclasses trust it. A
  config built with `GPTConfig(n_embd=7.5)` voids the guarantee, as a bad argument to any checked
  function does.
- A config class's own requires aren't modeled: there are none, since a dataclass's `__init__` only
  stores. What the modules need (`n_embd >= 0`) is inferred as their constructors' requires, as before.

## Other changes

- **Stubs:** `nn.GELU`, without `approximate`, since stubs can't take strings yet.
- **Notes:** a dataclass is no longer "skipped" with a note. Its annotated methods still are.

## Tests

- `frontend/tests/test_translate.py` (`Configs`): `__init__`'s signature (fields named by field, the flag
  included, floats and other fields left out), the assert as an ensures about the fields, a method's
  config-sourced instance dims and its `instances`, field reads in `__init__`, `self.config.n` in a
  method, passing a config on in `__init__` and computing a nested module's dims in a method, locals
  named like fields, and the errors (a flag in a method, a config as a value, building a config, other
  fields, missing fields, name clashes, `Optional` configs, and a non-config argument).
- `examples/pass/configs.py`: a feed-forward block and a tagger built from one config, with a stored
  config read in a method. It also runs under jaxtyping's runtime checker. `test_examples.py` pins that
  only the constructors infer requires. `examples/fail/config_dims.py` swaps an `nn.Linear`'s sizes.
- `frontend/tests/test_gpt_goal.py`: `PASSING` has 4 methods.

## Limitations

- **Only top-level dataclasses**, with fields annotated `int`, `bool` or `float` by name. Inherited
  fields aren't read. A config can't be `Optional`, have a default, be returned, or be stored in a local.
- **Flags aren't known in methods.** `self.config.bias` is an error there. Milestone 2 decides how a flag
  decides an attribute's `None`-ness (N5).
- As for modules, a constructor argument can't be a local of `__init__`: `width = 4 * config.n_embd;
  self.up = nn.Linear(config.n_embd, width)` is an error in the methods that use `self.up`.
- A config parameter of a method or a free function works, but its fields can't have the names of the
  instance dims, which rules out passing a module's own config class again.
