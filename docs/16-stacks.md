# Transformer milestone 4: stacks and encodings

**Gap:** the last 7 targets needed loops over an `nn.ModuleList` (G5), attributes declared at class level,
`register_buffer` and weight tying (G9), and slicing with symbolic bounds and strides (G10). See
[12-transformer-goal.md](12-transformer-goal.md). Now all 24 check: `PositionalEncoding`, `Encoder` and
`Decoder` (`__init__` and `forward` of each), and `Transformer.__init__`.

There are two changes to the core: a `Loop` statement and a `Slice` term. Declared attributes need no core
change.

## Declared attributes (G9)

`PositionalEncoding` builds its table in a local and registers it:

```python
class PositionalEncoding(nn.Module):
    pe: Float[Tensor, "1 max_len d_model"]

    def __init__(self, d_model: int, dropout: float, max_len: int = 5000):
        ...
        pe = torch.zeros(max_len, d_model)
        ...
        self.register_buffer("pe", pe.unsqueeze(0))
```

Milestone 2 typed an attribute by evaluating its one assignment again from the instance dims. That can't
work here: `pe.unsqueeze(0)` depends on the local `pe`. So the class-level annotation is the attribute's
type:

- **A declaration** is a class-level annotation with no value, e.g. `pe: Float[Tensor, "1 max_len
  d_model"]`. It may only name instance dims, since every instance has the attribute. Any other name is an
  error, because it would be existential with nothing to bind it.
- **Reading** `self.pe` in any method gives a value of the declared type, over the method's instance dims.
- **Every assignment is checked** against the declaration: `self.pe = v`, and `self.register_buffer("pe",
  v)`, which is the same assignment. Assignments are still only allowed in `__init__`, but one may appear
  more than once, since each is checked.

The frontend lowers these to two functions of the instance dims, put in the environment like stubs. The
getter `PositionalEncoding.pe(d_model, max_len) -> [1, max_len, d_model]` reads the attribute, and the
setter `PositionalEncoding.pe (assigned)(d_model, max_len, value: [1, max_len, d_model]) -> None` assigns
it. Both list the class's instance, so they assume its invariant. Because the declared shape's names are
int parameters, calling the setter checks that the value's shape equals the declared one. That's
milestone 1's rule for dims named after ints.

**Stub classes declare attributes the same way.** In `stubs/torch/nn/__init__.pyi`:

```python
class Linear(Module):
    weight: Float[Tensor, "out_features in_features"]

class Embedding(Module):
    weight: Float[Tensor, "num_embeddings embedding_dim"]
```

That's what weight tying needs. In `Transformer.__init__`, `self.proj.weight = self.embed.lut.weight`
calls `Linear.weight (assigned)` with `self.proj`'s dims `(d_model, vocab)`, on `Embedding.weight` with
`self.embed.lut`'s dims `(vocab, d_model)`: both are `[vocab, d_model]`. Swap the projection's arguments
and it's an error:

```
tie_shape.py:13: in Tied.__init__, `self.out.weight = self.embed.weight`: torch.nn.Linear.weight
(assigned): ... expected out_features = d, got vocab
```

`bias` isn't declared, since whether it exists depends on the `bias` flag.

## Loops over an `nn.ModuleList` (G5)

```python
self.layers = nn.ModuleList([EncoderLayer(d_model, d_ff, h, dropout) for _ in range(n_layers)])
...
for layer in self.layers:
    x = layer(x, mask)
return x
```

**The list:** in `__init__`, `nn.ModuleList([C(args) for _ in range(n)])` is recognized when the
comprehension has one generator over `range`, no `if`, and an element that doesn't use the loop variable.
Every module in the list is then built by the same call, so one instance, with that call's dims, stands
for all of them. `__init__` checks the constructor call once. When `n` is 0 no module is built, so this
asks for slightly more than the program needs (e.g. `h >= 1` for no layers).

**The loop:** `for layer in self.layers:` binds `layer` to that instance, and the body is translated once.
The invariant of the loop is that every local the body reassigns keeps its shape. The checker checks the
body once, starting from the shapes before the loop, and then checks that those shapes are restored. By
induction, any number of iterations (including none) leaves them unchanged, so after the loop each local
has its shape from before. This is what G5 described as "the loop variable's declared shape is the
invariant": `x`'s shape before the loop is its parameter's annotation.

The core has a new statement, `Loop (carried, body)`. `carried` pairs each reassigned local's name before
the body with its name after, since the frontend may rename a local that shadows an instance dim.
Checking it:

- The body is checked in a solver scope. What it assumes (an `assert`, the unfolding of a list variable)
  only holds if the body runs, so none of it is kept afterwards. Neither are the locals it binds.
- A carried local must end the body with the same shape: each dim provably equal, and the same list
  variables in the same places. An int must keep its value.
- `return` inside a loop is an error, so the statements after the loop are always checked.
- A precondition inferred inside the body is a requires, which holds after the loop too. Popping the
  scope removed it from the solver, so it's asserted again. Otherwise a later statement couldn't prove it
  and couldn't infer it either, since it's already been inferred.

```
loop_shape.py:24: in Stack.forward, `for layer in self.layers: ...`: `x` doesn't keep its shape in the
loop: array of shape [b, d] before the body, array of shape [b, 2 * d] after
```

In the frontend, a name the body binds, and the loop variable, may be unbound after the loop (if it didn't
run) or hold anything, so using one after the loop is an error. `break`, `continue` and `for ... else`
aren't supported. The body may reassign a local but not a module parameter.

## Slicing (G10)

`x[a:b:c, ...]` slices `x`'s leading dims. The new core term `Slice (x, items)` computes each sliced dim as
Python does, for a positive step:

- a negative bound counts from the end: `a + n` for `a < 0`;
- both bounds are clamped to `[0, n]`;
- the length is `(b - a + c - 1) // c` if `b >= a`, and 0 otherwise.

Each case is expressed with Z3's `ite`, but a case the solver can already decide is taken directly. So
`pe[:, :n]` after `assert n <= max_len` is just `n`, and `x[::2]` is `(n + 1) // 2`. A slice that stays an
`ite`, like `pe[:, :n]` without the assert, which is `min(n, max_len)`, is labeled by its source:
`max_len[:n]`. A bound that's an int of unknown value gives a length of at most `n`. torch doesn't allow a
step of 0 or less, so the step must be provably positive.

This is exact, not an approximation, so no entry type (`Min`) is needed: the dims are Z3 terms. The cost
is that a sliced dim that stays an `ite` isn't an arithmetic dimension that `-1` inference can factor. The
goal doesn't need that.

**Slice assignment** `pe[:, 0::2] = torch.sin(position * div_term)` writes into `pe`, which keeps its
shape. The value must broadcast to the slice. The frontend calls a stub, passing the slice itself:

```python
# operator.pyi
def setitem(target: Shaped[Tensor, "*A"], value: Shaped[Tensor, "*#A"]) -> None: ...
```

`*#A` is milestone 3's "broadcasts to". In `PositionalEncoding.__init__`, `pe[:, 0::2]` has
`(d_model + 1) // 2` columns and `pe[:, 1::2]` has `d_model // 2`, and the value has `(d_model + 1) // 2`
(from `torch.arange(0, d_model, 2)`). These are equal because of `assert d_model % 2 == 0`. Without the
assert:

```
slice_width.py:19: in Sinusoids.__init__, `table[:, 1::2] = torch.cos(angles)`: operator.setitem: ...
[max_len, (d + 1) // 2] doesn't broadcast to *A = [max_len, d[1::2]]
```

In `PositionalEncoding.forward`, `assert x.size(1) <= self.pe.size(1)` makes `self.pe[:, : x.size(1)]` a
`[1, n, d_model]` array that `x` broadcasts with. Without the assert, the rows are `min(n, max_len)`:

```
in PositionalEncoding.forward, `return self.dropout(x + self.pe[:, : x.size(1)])`: operator.add: ...
Cannot compute Broadcasted(A, B) for A = [b, n, d_model], B = [1, max_len[:n], d_model]
```

## Stubs

- `torch.arange(start, end, step=1)`, with length `(end - start + step - 1) // step`. It requires
  `start <= end` and `step >= 1`, which is what torch checks for a positive step.
- `torch.sin`, `torch.cos`.
- `operator.setitem` for slice assignment, above.
- `weight` declared on `nn.Linear` and `nn.Embedding`.

## Soundness notes

- **Declared attributes:** every assignment is checked against the declaration, and assignments only
  happen in `__init__` (the frontend rejects others). So a read in any method gives the declared shape.
  After weight tying, `proj.weight` still has `Linear`'s declared shape, so `Linear.forward`'s signature
  stays valid. Code outside the class can still assign an attribute, as in milestone 2.
- **`nn.ModuleList`:** every element is built by the same constructor call, from the same instance dims,
  because the element can't use the loop variable. So all elements are the same instance as far as shapes
  go. Checking that call once is stricter than needed when the list is empty.
- **Loops:** the body is checked from the pre-loop shapes and must restore them, so the pre-loop shapes
  are an inductive invariant. Facts from the body are scoped, so a body that never runs assumes nothing.
  Inferred facts are requires, which callers prove, so asserting them again after the loop is sound.
- **Slices:** the length is Python's formula for a positive step, with every case either decided by the
  solver or kept as an `ite`. An unknown bound only gives an upper bound on the length.
- **Slice assignment:** torch requires the value to broadcast to the slice, and `*#A` checks exactly
  that. The target keeps its shape.

## Other changes

- **Diagnostics:** a returned dim computed by arithmetic is labeled by its simplified value, so errors
  print `(d + 1) // 2` rather than a solver variable. A dim labeled by an expression is parenthesized as
  an operand.
- `Transformer.encode`, `decode` and `forward` now infer `b >= 1, d_model >= 1`, which the attention
  layers need for their `-1`s. Before, `Encoder.forward` and `Decoder.forward` didn't check, so their
  callers saw signatures with no requires.
- `test_goal` is no longer an expected failure. `PASSING` has all 24 functions.

## Tests

- `checker/test/test_bodies.ml`: a loop whose body keeps `x`'s shape, one that doesn't, a carried spread,
  a carried int, a local bound in the body being unbound after it, `return` in a loop, an assert in a body
  not holding after the loop, and a fact inferred in the body holding after it (without inferring a
  second one). Slices: `x[:2]` with and without `n >= 2`, `x[-2:]`, `x[::2]`, `x[1::2]`, `x[0::2]`
  against `x[1::2]` with and without even `n`, a symbolic stop within and past the end, a step of 0, too
  many slices, and a spread with unknown dims.
- `frontend/tests/test_translate.py` (`Attributes`, `Loops`, `Slices`): declarations in stubs and user
  classes, setters for assignments and `register_buffer`, getters for reads, weight tying's IR, errors
  for bad declarations and undeclared stub attributes; the `ModuleList`'s constructor call, the `Loop`
  and its carried locals, and each loop error; slice terms, slice assignment, and indexing errors.
- `examples/pass/stacks.py`: a strided `downsample`, sinusoid features in a registered buffer, and a
  tied language model over an `nn.ModuleList` of residual blocks. It also runs under jaxtyping's runtime
  checker. `examples/fail/tie_shape.py` ties a weight of the wrong shape, `examples/fail/loop_shape.py`
  has a layer that widens its input, and `examples/fail/slice_width.py` drops `assert d % 2 == 0`. All
  three fail at run time too.
- `frontend/tests/test_goal.py`: all 24 pass.

## Limitations

- **Loops:** only over an `nn.ModuleList` attribute built by a comprehension whose elements are alike. No
  `range` loops, `while`, `break`, `continue` or `for ... else`. A local the body reassigns must keep its
  exact shape. A body that needs a weaker invariant, like a growing length, isn't supported.
- **Indexing:** only slices, one per leading dim. Int indices (`x[0]`), `None`, `...` and advanced
  indexing aren't supported, nor are negative steps (torch doesn't allow them either). Only slices of
  local variables can be assigned.
- **Declared attributes** may only name instance dims, and only arrays and ints can be declared. A module
  can't be declared.
- A sliced dim that stays an `ite` can't be factored by `-1` inference in a later `view` or `reshape`.
