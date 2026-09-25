# Transformer milestone 2: modules and instance dims

**Gap:** classes were skipped, so none of the Transformer's `nn.Module`s checked (see
[12-transformer-goal.md](12-transformer-goal.md), G1 and G2). A method's annotation like `"b n d_model"`
refers to the `d_model` given to `__init__`, and `self.w_1(x)` needs the type of `self.w_1`. Now 13 of 24
check. Milestone 2's targets are `PositionwiseFeedForward`, `Embeddings`, `EncoderLayer.__init__`,
`DecoderLayer.__init__` and `Transformer.encode`/`decode`/`forward`, and `MultiHeadedAttention.__init__`
checks too.

## Design: modules are lowered to their ints

The decision (12-transformer-goal.md, decision 2) is the constructor-int convention: a dim named after an
int parameter of `__init__` is that int, in every method of the class, for user classes and stub classes
alike. There's no new annotation syntax.

The frontend implements it by lowering modules away, so the core has no objects:

- **An instance is its instance dims:** the int arguments its `__init__` was called with. Only parameters
  annotated `int` count. `bool`s are flags, and floats (`dropout`) don't affect shapes.
- **A method is a function of its instance dims.** `PositionwiseFeedForward.forward` becomes the IR function
  `PositionwiseFeedForward.forward(d_model: int, d_ff: int, x: [b, n, d_model])`. That's milestone 1's
  rule, where a dim named after an int parameter is its value, so `d_model` in the annotation needs
  nothing new.
- **An attribute's type is its one assignment in `__init__`.** Given `self.w_1 = nn.Linear(d_model, d_ff)`,
  the call `self.w_1(x)` in any method becomes `torch.nn.Linear.forward(d_model, d_ff, x)`. The arguments
  are bound by Python's rules against `Linear.__init__` (keywords, defaults), and each of `Linear`'s
  instance dims is translated from `__init__`'s names to the method's instance dims. Nested modules
  compose by substitution. `EncoderLayer`'s `self.self_attn = MultiHeadedAttention(h, d_model, dropout)`
  gives `MultiHeadedAttention.forward(h, d_model, ...)`, and `Outer`'s `self.block = Block(2 * d, 4)`
  gives `Block.forward(2 * d, 4, ...)`.
- **Other attributes are their expressions, evaluated again.** `self.d_k = d_model // h` makes `self.d_k`
  in a method the term `d_model // h` over the method's instance dims. The expression's only inputs are
  instance dims, so it has the same value, or the same shape for a tensor like `torch.randn(d, n)`. It
  may not mention anything else from `__init__`: `e = 2 * d; self.w = nn.Linear(d, e)` is an error in
  the methods that use `self.w`.
- **Calls:** `module(x)` is `forward`, `self.f(x)` calls the attribute's `forward` or else `self`'s method
  `f`, and `self.block.widen(x)` calls a method of an attribute. A constructor call in `__init__` checks the
  constructor's signature and returns `None`.
- **`__init__`** returns `None`. `super().__init__()` is skipped, and falling off the end returns
  `None`. A function annotated `-> None` works the same way.

Stub classes use the same convention (`frontend/stubs/torch/nn/__init__.pyi`):

```python
class Linear(Module):
    def __init__(self, in_features: int, out_features: int, bias: bool = True) -> None:
        assert in_features >= 0 and out_features >= 0
    def forward(self, input: Float[Tensor, "*B in_features"]) -> Float[Tensor, "*B out_features"]: ...
```

A stub class that subclasses `Module` is a module class. Its methods only go in the IR as calls on its
instances, not in the method tables used for `x.sum()`.

### The alternative: objects in the core

The core could have had object types: a `TypeObject` of a class, values carrying their fields,
attribute terms and method dispatch. That would move work the frontend does statically into the core, and
the core would still need the class's field types, which come from `__init__`'s syntax. Lowering keeps
the core to one new concept (below), and reuses int parameters as dims, which the core already has.

## Class invariants: the one core change

Every instance was built by its constructor, so its instance dims satisfy the constructor's requires. A
method may assume them. So may its callers, which only ever pass an instance's dims.

Without this, `self.head(x)` for `self.head = nn.Linear(d_model, n_classes)` returns `[b, n, n_classes]`,
and `n_classes >= 0` isn't provable in `forward`: nothing in its inputs mentions `n_classes`. Every method
up the call chain would infer `n_classes >= 0` as a requires. With invariants, `Linear`'s constructor
requires `out_features >= 0`, so every `Linear` satisfies it and the call assumes it.

- **Core:** a signature has an `invariant`, constraints over its int parameters. `check_body` assumes it
  alongside the requires. `check_sig` assumes it once the arguments match, before proving the requires.
  A constraint the solver can't state (a quotient whose divisor isn't provably positive) is dropped,
  which only assumes less.
- **IR:** a signature may list `instances`, each naming a constructor and pairing its parameters with the
  signature's (`{"init": "torch.nn.Linear.__init__", "ints": [["in_features", "in_features"], ...]}`). A
  method has one, for `self`. The CLI builds the invariant each round from the constructor's requires,
  renamed, including the requires inferred for user constructors so far. A fact about anything that isn't
  an instance dim is dropped.
- **Constructors first:** the CLI first runs its rounds over the constructors alone, then over
  everything. Otherwise a method's first round wouldn't see what its instances' constructors infer, and
  would infer the same fact as a requires of its own. Facts only accumulate, so it would keep it.
  Without this phase, `Transformer.encode` would require `d_model >= 0`.

In the goal, `Embeddings.__init__` infers `vocab >= 0, d_model >= 0` from `nn.Embedding`'s requires. So
`Transformer.decode`'s call `self.embed(tgt)` assumes `d_model >= 0`, and `decode` infers nothing.
`Transformer.forward` still infers `d_model >= 0`: it calls `self.encode` first, and `Transformer.__init__`
doesn't check yet (milestone 4), so it has no invariant. Asserts in `__init__` will add to the invariant
in milestone 3 (G7).

## Soundness notes

- A method is checked for every instance its class's `__init__` can build, provided `__init__` satisfied
  its requires. A caller that breaks a constructor's requires voids its instances' guarantees, as with any
  precondition.
- An attribute's type is sound only if the one top-level assignment in `__init__` is its value when a
  method runs:
  - An attribute assigned more than once, or anywhere other than the top level of `__init__` (in a
    branch, or a tuple target), is an error in the methods that use it.
  - Assigning an attribute outside `__init__` is an error.
  - `__init__` can't reassign one of its int parameters, since attributes are read in terms of them.
  - Assigning to an attribute of an attribute (`self.proj.weight = ...`) is an error. Weight tying is
    milestone 4 (G9).
  - In-place ops that change shapes have no stubs.
  - Code outside the class can still replace an attribute (`model.w = ...`). The checker assumes it
    doesn't.
- Evaluating an attribute's expression again in a method proves its preconditions again. That can fail
  where `__init__` succeeded, but it never assumes anything. A data-dependent shape (`torch.unique`) gets
  a fresh size each time, so less is known.
- The frontend passes instance dims only from attribute assignments and `self`. The core trusts them, and
  assumes the callee's invariant about them.

## Other changes

- **Diagnostics:** when a spread is followed only by single dims, and the argument's dims are all known,
  only one split can match. The matcher now tries that split first, so a mismatch is reported where it is:
  `expected in_features = d_ff, got d_model` rather than `got b` from the split that gave `*B` nothing.
- **Stubs:** `nn.Linear`, `nn.Embedding`, `nn.LayerNorm` (an int `normalized_shape`), `nn.Dropout`,
  `nn.ReLU`, and `math.sqrt`/`math.log` (an int argument must be `>= 0`/`>= 1`).
- **Notes** now have the right line for methods, and the checker prints `None` for the empty tuple.

## Tests

- `checker/test/test_bodies.ml`: a call assumes the callee's invariant (and without one, the returned dim
  may be negative), a body assumes its own, invariants don't leak out of the body, a quotient that can't be
  stated is dropped, and an invariant may only mention parameters.
- `frontend/tests/test_translate.py` (`Modules`): method signatures, `__init__`'s body, attribute calls,
  `self` calls, int attributes, nested modules with substituted dims, the stub invariant in `env`, locals
  named like instance dims, stub module classes, and each error.
- `examples/pass/modules.py`: a residual feed-forward block with a derived width
  (`self.hidden = expansion * d_model`), and a classifier with an embedding, a nested block and a `self`
  method. It also runs under jaxtyping's runtime checker. `frontend/tests/test_examples.py` pins that only
  the constructors infer requires. `examples/fail/module_linear_dims.py` swaps `nn.Linear`'s arguments,
  and `examples/fail/module_nested_dims.py` builds a nested module for the wrong size.
- `frontend/tests/test_goal.py`: `PASSING` has 13 functions.

## Limitations

- **Modules as values:** a module can't be passed as an argument (`attention(..., dropout=self.dropout)`,
  milestone 3), returned, or stored in a local. A parameter annotated with a module class would lower to
  that class's instance dims, like `self`.
- **Only direct `nn.Module` subclasses.** A user class that subclasses another user class is skipped with a
  note, and so are other classes. Decorated methods (`@staticmethod`, `@property`) aren't supported.
- **Attributes are ints, tensors computed from instance dims, and modules.** Class-level annotations
  (`pe: Float[...]`), `register_buffer`, `nn.ModuleList` and tensor attributes of stub modules
  (`self.proj.weight`) are milestone 4.
- **Derived attributes aren't dims.** An annotation can name `d_model` but not `d_k` from
  `self.d_k = d_model // h`. `self.d_k` works in bodies.
- An overloaded stub `__init__` (`LayerNorm`'s tuple `normalized_shape`) isn't supported.
