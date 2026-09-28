# shapecheck: the Python frontend

`shapecheck` statically checks Python functions annotated with
[jaxtyping](https://github.com/patrick-kidger/jaxtyping) shape strings. Each function body is checked
once, and the result holds for every caller. The frontend only translates: it reads the Python with the
stdlib `ast` module, emits the checker's IR as JSON, and runs the OCaml checker (`checker/bin`) on it.

```python
import torch.nn.functional as F
from jaxtyping import Float
from torch import Tensor


def mlp(x: Float[Tensor, "*batch d"], w: Float[Tensor, "d h"]) -> Float[Tensor, "*batch h"]:
    return F.relu(F.linear(x, w))
```

```
$ python -m shapecheck mlp.py
mlp.py:7: in mlp, `return F.relu(F.linear(x, w))`: torch.nn.functional.linear (overload 1): Could not
type check: parameter weight (array[p, k], given array of shape [d, h]): expected k = d, got h
```

`F.linear` takes its weight as `[out, in]`, so `w` should be `"h d"`.

## Usage

Build the checker once, then run the frontend from this directory. It needs Python 3.9+ and has no
dependencies.

```sh
(cd ../checker && dune build)
python -m shapecheck FILE...               # check; exit 1 if anything fails
python -m shapecheck --dump-ir FILE        # print the IR instead
python -m shapecheck --stubs DIR FILE      # add a stub directory
```

`pip install -e .` also installs a `shapecheck` command. The checker is found at
`checker/_build/default/bin/shapecheck.exe`, or wherever `SHAPECHECK_CHECKER` or `--checker` points.

Tests:

```sh
python -m unittest discover -s tests -t .  # needs the checker built, for tests/test_examples.py
uvx ruff format . && uvx ruff check .
```

## What gets checked

Every top-level function with at least one annotation, and every such method of an `nn.Module` subclass
(see [Modules](#modules)). A `@dataclass` is a config (see [Configs](#configs)), and other classes are
skipped with a note. Every parameter and the return type need annotations, except `self` and
`__init__`'s return type:

| Annotation | Checker type |
|---|---|
| `Float[Tensor, "*batch n d"]` (any jaxtyping dtype, any array type) | an array of that shape |
| `int`, `bool` | an int |
| `Literal[3]`, `Literal[True]` | that int (bools are 0 and 1) |
| `float` | a 0-d array: a float broadcasts like one |
| `Tensor`, `np.ndarray` with no shape | a parameter of any shape (not allowed as a return type) |
| `tuple[A, B]` (return types only) | a tuple of those types |
| `None` (return types only) | the empty tuple |
| `Optional[T]`, `T \| None` in a parameter | `None` or a `T`: checked once for each (see [Optional](#optional-parameters)) |
| `Optional[T]`, `T \| None` in a return type | `None` or a `T`: a caller may unpack it or return it, but not use it otherwise |
| `nn.Dropout`, a user's `nn.Module` subclass | an instance of that module (see [Modules](#modules)) |
| a `@dataclass` like `GPTConfig` | its `int` and `bool` fields, as ints named by field (see [Configs](#configs)) |

Shape strings follow jaxtyping:

| jaxtyping | Meaning |
|---|---|
| `b`, `seq`, `3` | a named or fixed dim |
| `*batch` | a named run of dims |
| `...`, `*_` | an unnamed run of dims (not in a return type) |
| `_`, `_foo` | a dim that isn't checked |
| `*#batch` | a run of dims that broadcasts to `batch`: no more dims, each 1 or `batch`'s. The first occurrence binds `batch` exactly |
| `#n` | `n` or 1. In a parameter, like `n`, it binds `n` where it first appears. In a return type, a parameter must bind `n` |
| `dim-1`, `2*dim`, `(n+1)//2` | arithmetic with `+ - * //` on names and ints |

A name that only the return type mentions is existential. Shape names and Python parameter names are
separate, as in jaxtyping, except for ints: a dim named after an int parameter is its value, so
`def causal_mask(size: int) -> Bool[Tensor, "size size"]` returns a `[t, t]` mask for `causal_mask(t)`.
Any other parameter whose name clashes with a dim is renamed in the IR (`n` becomes `n'`).

**Inferred preconditions.** torch raises on sizes like `torch.ones(-1)`, so rather than reject a body that
can't prove a size is valid, the checker infers what callers must pass. The candidates are an int
parameter being `>= 0` or `>= 1`, and a dim being `>= 1`. Callers then have to prove these, or infer
them in turn:

```
sizes.py:7: note: causal_mask requires size >= 0 (inferred from its body)
```

Only size obligations are inferred: an int used as a size, a returned dim, the other sizes of a `-1`
(torch can't infer `-1` if they multiply to 0), an index being in range (`x[:, -1]` needs the dim to be
`>= 1`), and a callee's inferred requires. One relation is
inferred too: a slice's stop being within its dim, relating two of the signature's names, when the
statement with the slice doesn't check without it. nanoGPT's mask `self.bias[:, :, :T, :T]` broadcasts
against the scores only if `t <= block_size`:

```
causal.py:31: note: CausalSelfAttention.forward requires d >= 1, n <= max_len (inferred from its body)
```

A caller may infer a callee's inferred relation in turn, or prove it from an assert. Declared relations,
like a kernel fitting the image, must be proved. See [docs/13-free-functions.md](../docs/13-free-functions.md)
and [docs/20-causal-attention.md](../docs/20-causal-attention.md).

Bodies must be straight-line code, apart from `if`s that are decided statically and loops over an
`nn.ModuleList`:

- `y = expr` and `y: Float[Tensor, "..."] = expr`. The annotation is checked, and it can bind new names.
- `a, b = expr`, unpacking a tuple. `B, T, C = x.size()` unpacks a tensor's dims, and `q, k, v =
  x.split(s, dim=2)` its pieces: the stubs have an overload per rank and per number of pieces, so the
  unpacking checks that there are that many.
- `return expr`, including `return a, b`. An item of a returned tuple may be `None` where the return
  type's is `Optional`, as in `return logits, None`.
- `if` on whether variables are `None` (`if mask is not None:`), which is known statically. See
  [Optional parameters](#optional-parameters). And `if` on a flag (`if not self.flash:`), which is
  checked once for each value. See [Flags](#flags).
- `assert`: the rest of the body assumes it. Comparisons of ints with `+ - * //` are stated, and so is
  `a % b == 0`, as `b * (a // b) == a`. A call in one, like `x.size(1)`, is evaluated first. Anything else
  in an assert is dropped, which is sound. A divisor may be inferred positive, like a size.
- `for layer in self.layers:` over an `nn.ModuleList`. The body is checked once, and the locals it
  reassigns must keep their shapes. See [Modules](#modules).
- `x[a:b] = v` assigns into a slice. `v` must broadcast to the slice, and `x` keeps its shape.
- `print(...)` is skipped, but the values it prints, other than strings and the text of f-strings, are
  checked.
- `pass` is skipped.
- `device = x.device` binds a value that isn't a shape, which can be passed where the stubs expect one
  (`torch.arange(0, t, dtype=torch.long, device=device)`). See [Stubs](#stubs).
- Expressions: local variables, int/bool/float literals, `float` of a constant (`float("-inf")`), calls, `+ - * / // ** @ & | ^`, unary `-` and
  `~`, comparisons, methods (`x.sum(-1)`, `x.size(-1)`), properties (`x.mT`), tuples, and tuples of ints
  as shapes (`x.reshape((n, d))`). One entry of a shape may be `-1` where the stub determines it, as in
  `x.reshape(-1, d)`. `x.shape` is a shape too, but only where one is expected: `torch.zeros(x.shape)`.
- Indexing `x[a:b:c, i, [j], ...]` of the leading dims. A slice has Python's rules: negative bounds count
  from the end, and bounds past the end are clamped. So `x[:, :n]` has `min(n, m)` columns, which is `n`
  after `assert n <= m`. The step must be positive. An int `i` drops the dim, and a list of ints keeps it
  with as many entries, so `x[:, [-1], :]` is the last position with its dim. Both must be in range. Only
  one list is supported, and not with ints, since torch's advanced indexing would move the dims.

Anything else gets an explicit error, and the rest of the file is still checked. That covers other control
flow, augmented assignment (`x += y`), indexing with `None`, `...` or a mask, `x.shape` as a value,
lambdas, and module-level values.

## Optional parameters

An `Optional[T]` parameter (or `Union[T, None]`, `T | None`) is `None` or a `T` at each call, and the
frontend always knows which: `None` is the literal, a default of `None`, or a local that's `None`. So a
function is checked once for each choice of which `Optional` parameters are `None`, and each `if x is not
None:` is decided in each case:

```python
def masked_softmax(
    scores: Float[Tensor, "*b q k"], mask: Optional[Bool[Tensor, "*#b #q k"]] = None
) -> Float[Tensor, "*b q k"]:
    if mask is not None:
        scores = scores.masked_fill(~mask, -1e9)
    return scores.softmax(dim=-1)
```

This is two checker functions, `masked_softmax` and `masked_softmax[mask=None]`. A `None` parameter isn't
in its case's signature, and a call goes to the case its arguments select. A function passes if every case
does, and errors name the case:

```
optional_none.py:12: in masked[mask=None]: `mask` is None here
```

Tests may combine `is None` and `is not None` with `not`, `and` and `or`, and `x if y is None else z` is
decided the same way, as is `self.x is None` for an attribute `__init__` assigns. A function may have at
most 4 `Optional` parameters (16 cases). They aren't supported on `__init__`. Stubs may have them. See
[docs/15-attention.md](../docs/15-attention.md).

## Modules

A subclass of `nn.Module` is typed by its constructor's ints, its *instance dims*: the parameters of
`__init__` annotated `int`. A dim named after one, in any method's annotations, is that int:

```python
class FeedForward(nn.Module):
    def __init__(self, d_model: int, d_ff: int, dropout: float = 0.1):
        super().__init__()
        self.w_1 = nn.Linear(d_model, d_ff)
        self.w_2 = nn.Linear(d_ff, d_model)

    def forward(self, x: Float[Tensor, "b n d_model"]) -> Float[Tensor, "b n d_model"]:
        return self.w_2(self.w_1(x).relu())
```

An attribute's type comes from its one assignment in `__init__`, so `self.w_1(x)` is `nn.Linear(d_model,
d_ff)`'s `forward`. The frontend lowers this away: a method becomes a function that takes its instance
dims first, and `self.w_1(x)` becomes `torch.nn.Linear.forward(d_model, d_ff, x)`.

- `module(x)` calls `forward`. `self.f(x)` calls an attribute's `forward` or `self`'s method `f`, and
  `self.a.m(x)` calls an attribute's method `m`.
- A module attribute is built by a constructor call, `self.f = Module(...)`. Its instance dims must be
  computed from the int parameters of `__init__` (or other such attributes), not from its locals.
- Any other attribute is its expression, evaluated again from the instance dims: `self.d_k = d_model //
  h` makes `self.d_k` the int `d_model // h`.
- An attribute must be assigned once, at the top level of `__init__`, or inside `if`s on flags, which
  are decided in each case (see [Flags](#flags)). It can't be assigned anywhere else, and `__init__`
  can't reassign an int parameter.
- `__init__` returns `None`, and `super().__init__()` is skipped.
- A method may assume its class invariant: the requires of `__init__`, including those inferred for it,
  and its asserts about its int parameters (`assert d_model % h == 0`). Every instance was built
  satisfying them. So may a call: `nn.Linear`'s constructor requires `out_features >= 0`, so
  `self.head(x)` returns a valid size without the method requiring it.
- A parameter annotated with a module class (`dropout: nn.Dropout`) takes an instance, like `self.dropout`,
  and is passed as its instance dims. It may be `Optional`.
- A class-level annotation declares an attribute's shape, over the instance dims. Reads give that shape,
  and every assignment is checked against it, including `self.register_buffer("pe", v)`. Declare an
  attribute whose value is built from `__init__`'s locals:

  ```python
  class PositionalEncoding(nn.Module):
      pe: Float[Tensor, "1 max_len d_model"]
  ```

  Stub modules declare theirs the same way (`nn.Linear.weight`), so weight tying,
  `self.proj.weight = self.embed.weight`, checks that the shapes agree.
- `self.layers = nn.ModuleList([Layer(d) for _ in range(n)])` is a list of modules built alike. `for
  layer in self.layers:` checks its body once, with `layer` as one of them. Each local the body reassigns
  must keep its shape, which makes the loop's invariant. A name only the body binds isn't known after the
  loop.
- `self.transformer = nn.ModuleDict(dict(wte=nn.Embedding(...), h=nn.ModuleList(...)))` (or with a dict
  literal whose keys are names) is a namespace of modules. `__init__` checks each entry's constructor,
  and `self.transformer.wte(idx)`, `for block in self.transformer.h:` and
  `self.transformer.wte.weight = self.lm_head.weight` work as they would for attributes of `self`. The
  dict itself can't be called or used as a value.

Modules can't be returned or stored in locals yet, and an annotation can't name a module parameter's
dims. Only direct subclasses of `nn.Module` are checked. See [docs/14-modules.md](../docs/14-modules.md),
[docs/16-stacks.md](../docs/16-stacks.md) and [docs/21-the-model.md](../docs/21-the-model.md).

## Configs

A module may be built from a config, a `@dataclass` of settings, as nanoGPT's are. Its `int` fields are
the instance dims, named by field:

```python
class MLP(nn.Module):
    def __init__(self, config: "GPTConfig"):
        super().__init__()
        self.c_fc = nn.Linear(config.n_embd, 4 * config.n_embd, bias=config.bias)
        self.c_proj = nn.Linear(4 * config.n_embd, config.n_embd, bias=config.bias)

    def forward(self, x: Float[Tensor, "b t n_embd"]) -> Float[Tensor, "b t n_embd"]:
        return self.c_proj(F.gelu(self.c_fc(x)))


@dataclass
class GPTConfig:
    n_embd: int = 768
    bias: bool = True
```

- A config parameter is its `int` and `bool` fields, as int parameters named by field. The annotation may
  be a forward reference (`"GPTConfig"`), and a field can't share a name with another parameter.
- `config.n_embd` is that field, and a `float` field is a float. `self.config = config` stores it, so
  `self.config.n_embd` works in methods.
- `bool` fields aren't instance dims, so a method can't read them. `__init__` can pass them on
  (`bias=config.bias`), but not test them yet.
- `Block(config)` passes a config on. A config can't be used as a value otherwise, or built in checked
  code.

See [docs/18-configs.md](../docs/18-configs.md).

## Flags

A flag is a `bool` a module's code branches on: a `bool` parameter of `__init__`, or an attribute that
`__init__` sets to one, to `hasattr(...)`, or to `not`/`and`/`or` of those. Its value isn't known
statically, so a method is checked once for each value of each flag it tests:

```python
class LayerNorm(nn.Module):
    def __init__(self, ndim: int, bias: bool):
        super().__init__()
        self.weight = nn.Parameter(torch.ones(ndim))
        self.bias = nn.Parameter(torch.zeros(ndim)) if bias else None

    def forward(self, input: Float[Tensor, "*b ndim"]) -> Float[Tensor, "*b ndim"]:
        return F.layer_norm(input, self.weight.shape, self.weight, self.bias, 1e-5)
```

`forward` reads `self.bias`, whose value tests `bias`, so it's checked as `LayerNorm.forward[bias=True]`
and `LayerNorm.forward[bias=False]`. In the second, `self.bias` is `None`, which selects `layer_norm`'s
case without a bias. The cases are found from the code: a flag is split on when a case first tests it, so
a flag tested only under another is only split there (`[self.fast=False,shift=True]`).

- Unlike an `Optional` parameter, a caller can't pick a case, so they all have the function's one
  signature, and that's all callers see. What one case infers is required of them all, and an assert in
  `__init__` is part of the class invariant only if every case reaches it.
- `self.use_bias = bias` is the flag `bias`, and `self.flash = hasattr(F, "scaled_dot_product_attention")`
  is a flag of its own, `self.flash`. Setting a flag isn't checked otherwise.
- `__init__` can't reassign a flag. Only `self`'s flags can be tested, not a nested module's.
- `x if flag else y`, `if flag:` and `self.x is None` are decided in each case. An error names the case:

  ```
  flag_none.py:15: in Affine.forward[bias=False]: `self.bias` is None here
  ```

- An attribute assigned inside `if`s in `__init__` has its value per case: exactly one assignment must
  run, so the mask registered under `if not self.flash:` exists in `forward[self.flash=False]`. Reading
  it in a case where no assignment runs is an error, as it would be an `AttributeError`:

  ```
  mask_case.py:18: in Masked.forward[self.fast=True]: `self.mask` isn't assigned in this case: `Masked.__init__` assigns it under `if not self.fast:`
  ```

- `self.training`, `nn.Module`'s flag, is a flag too, unless `__init__` assigns it. `train()` and
  `eval()` set it between calls, so each call is in one case. Used as a value, it's its case's value.
- A function may test at most 4 flags (16 cases, for each case of its `Optional` parameters).

See [docs/19-flags.md](../docs/19-flags.md) and [docs/20-causal-attention.md](../docs/20-causal-attention.md).

## Calls

Calls resolve through the file's imports (`import torch.nn.functional as F`, `from torch import
relu`) to user functions, in any order and including recursion, or to stubs. Methods and properties
resolve to the stub classes. Operators go to the stub module `operator`: `x @ w` becomes
`operator.matmul(x, w)`. The frontend binds keyword and default arguments with Python's rules, so the
checker only sees positional calls. When some of a stub's overloads don't accept the arguments, the call
goes only to those that do, e.g. `torch.Tensor.sum (overload 2)`.

## Stubs

Library signatures are `.pyi` files under `stubs/`. `stubs/torch/nn/functional.pyi` is the module
`torch.nn.functional`, and methods and properties live in classes (`class Tensor` in
`stubs/torch/__init__.pyi`). Repeated definitions of a name are overloads, tried in order. Stubs use the
same syntax as user code, plus what library signatures need and jaxtyping can't express:

- A shape may name an int parameter: `def sum(self: Shaped[Tensor, "*A"], dim: int) -> Shaped[Tensor,
  "*drop(A,dim)"]`.
- List functions: `*drop(A,i)`, `*keep(A,i)`, `*permute(A,i,j)`, `*swap(A,i,j)`, `*setat(A,i,d)`,
  `*insertat(A,i,d)`, `*broadcast(A,B)`. Inside arithmetic, `prod(A)`, `rank(A)`, and `A[i]` (one dim, e.g.
  `-> Dim["A[dim]"]` for `x.size(dim)`).
- `Dim["expr"]` is an int equal to a dim expression, e.g. `-> Dim["rank(A)"]` for `x.dim()`.
- `Shape["*S"]` is a tuple of ints used as a shape. As `*size: Shape["*S"]` it collects int arguments, so
  `torch.zeros(n, d)` and `torch.zeros((n, d))` both work. One entry may be `-1` if an equation the stub
  asserts determines it, like reshape's `assert prod(A) == prod(B)`: the other sizes must have a positive
  product that divides the total. Any other negative entry is an error, and a possibly negative one
  needs an inferred precondition.
- Asserts in the body are preconditions (`assert prod(A) == prod(B)`), or postconditions if they mention
  an existential (`assert m <= n` for `unique`).
- `Optional[T]` parameters, as in user code: `layer_norm`'s `weight` and `bias`. A call with `None` goes to
  the stub's case without it, e.g. `torch.nn.functional.layer_norm[bias=None]`.
- A class with nothing in it, like `class device: ...`, is a kind of value that isn't a shape. A parameter
  annotated with one (`device: Optional[device] = None`) takes such a value, or `None`, and isn't passed to
  the checker, so it doesn't make cases. A module-level annotation declares a constant of one (`long:
  dtype` for `torch.long`), and one in a class declares an attribute of arrays (`device: device` in
  `Tensor`, for `x.device`, which is only supported on a local variable).
- A class that subclasses `Module` is a module class, typed like a user's module: its instance dims are
  the `int` parameters of `__init__`, and its methods' annotations may name them. Asserts in `__init__`
  are the constructor's requires, which its instances then satisfy:

  A class-level annotation declares an attribute:

  ```python
  class Linear(Module):
      weight: Float[Tensor, "out_features in_features"]

      def __init__(self, in_features: int, out_features: int, bias: bool = True) -> None:
          assert in_features >= 0 and out_features >= 0

      def forward(
          self, input: Float[Tensor, "*B in_features"]
      ) -> Float[Tensor, "*B out_features"]: ...
  ```

The shipped stubs cover common torch functions, `Tensor` methods, `torch.nn.functional`, the modules
`nn.Linear`, `nn.Embedding`, `nn.LayerNorm`, `nn.Dropout`, `nn.ReLU` and `nn.GELU`, `nn.Parameter`,
`math.sqrt`/`math.log`, `F.scaled_dot_product_attention`, and Python's operators. `x.size()` and
`x.split(s, dim)` have an overload per rank (1 to 6) and per number of pieces (1 to 4). `Tensor.shape` is a property whose value is a shape
(`-> Shape["*A"]`), and the frontend only allows it where a shape is expected. `operator.setitem(target, value)` is slice assignment: the frontend passes the slice
itself as `target`. `torch.dtype` and `torch.device` are kinds of values, with the dtypes as constants, and
the creation functions take `dtype` and `device`. `nn.ModuleList` and `nn.ModuleDict` have no stubs; the
frontend handles them. A call with no stub is an error, never an unknown shape.

## The IR

The JSON mirrors the OCaml types in `checker/src/typing.ml`. Each value is a list tagged with its
constructor's name. `checker/bin/ir_json.ml` decodes it.

```
entry  ["Id", n] ["Int", i] ["Add"|"Sub"|"Mul"|"Div", e, e] ["Spread", A] ["Broadcast", A]
       ["BroadcastDim", n] ["Broadcasted", [A, ...]] ["Drop"|"Keep"|"Permute", A, [idx]]
       ["Swap", A, idx, idx] ["SetAt", A, [idx], e] ["InsertAt", A, idx, e] ["Prod", A] ["Rank", A]
       ["Index", A, idx]
                                                                 idx is a name or an int
typ    ["Array", [entry]] ["Int"] ["IntExpr", entry] ["Literal", i] ["Tuple", [typ]]
       ["Optional", typ]                                         return types only
constr ["Eq"|"Le"|"Lt", entry, entry]
sig    {"params": [[name, typ]], "ret": typ, "requires": [constr], "exists": [name], "ensures": [constr],
        "invariant": [constr], "instances": [{"init": f, "ints": [[init_param, param]]}]}
term   ["Var", x] ["Lit", i] ["Call", f, [term]] ["Shape", [term]] ["Scalar"] ["Tuple", [term]]
       ["Slice", term, [item]]
item   [start, stop, step] ["Index", term] ["List", [term]]   start, stop, step are terms or null
stmt   ["Let", x, term] ["LetAnnot", x, typ, term] ["Unpack", [x], term] ["Return", term]
       ["Assume", constr] ["Loop", [[x_before, x_after]], [stmt]] ["At", line, text, stmt]

program {"env": [{"name": f, "overloads": [sig]}],
         "functions": [{"name": f, "sig": sig, "body": [stmt] | null, "group": f}]}
```

`None` is `["Tuple", []]`, which an `Optional` also accepts. A signature's `invariant` and `instances` are optional, and so is a function's
`group`. An invariant is
assumed by the body and by callers. Each instance says that some of the parameters are an instance's dims:
the CLI adds the constructor `init`'s requires and ensures, renamed to those parameters, to the invariant.
A method has one instance, for `self`, and each module parameter adds one. `Assume` is an assert: the
rest of the body assumes the constraint, whose names are the body's int locals. `Loop` is a body that
runs any number of times: each pair names a local before and after the body, which must keep its shape.
Afterwards the locals are as before the loop.

A declared attribute `a` of a class `C` is two functions in `env`: `C.a` takes the instance dims and
returns the declared type, and `C.a (assigned)` takes the instance dims and the value, of the declared
type, and returns `None`.

A function with `Optional` parameters is one checker function per case, named like
`attention[mask=None]`, and so is a stub's case. A function that tests flags is a function with its
signature and no body, which callers use, and a checker function per case, named like
`LayerNorm.forward[bias=False]`, whose `group` names it. The cases have its signature, and the CLI keeps
their inferred requires per group, so what one case infers is required of all of them and seen by
callers.

A function whose body couldn't be translated has `"body": null`, so callers still use its signature. The
checker prints `{"results": [{"name": f, "error": null | message, "inferred": ["n >= 0", ...]}]}` and
exits 0 if everything checks, 1 if something doesn't, and 2 for invalid IR. `inferred` lists the
preconditions added to `f`'s signature.
