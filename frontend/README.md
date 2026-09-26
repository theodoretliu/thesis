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
(see [Modules](#modules)). Other classes are skipped with a note. Every parameter and the return type need
annotations, except `self` and `__init__`'s return type:

| Annotation | Checker type |
|---|---|
| `Float[Tensor, "*batch n d"]` (any jaxtyping dtype, any array type) | an array of that shape |
| `int`, `bool` | an int |
| `Literal[3]`, `Literal[True]` | that int (bools are 0 and 1) |
| `float` | a 0-d array: a float broadcasts like one |
| `Tensor`, `np.ndarray` with no shape | a parameter of any shape (not allowed as a return type) |
| `tuple[A, B]` (return types only) | a tuple of those types |
| `None` (return types only) | the empty tuple |
| `Optional[T]`, `T \| None` (parameters only) | `None` or a `T`: checked once for each (see [Optional](#optional-parameters)) |
| `nn.Dropout`, a user's `nn.Module` subclass | an instance of that module (see [Modules](#modules)) |

Shape strings follow jaxtyping:

| jaxtyping | Meaning |
|---|---|
| `b`, `seq`, `3` | a named or fixed dim |
| `*batch` | a named run of dims |
| `...`, `*_` | an unnamed run of dims (not in a return type) |
| `_`, `_foo` | a dim that isn't checked |
| `*#batch` | a run of dims that broadcasts to `batch`: no more dims, each 1 or `batch`'s. The first occurrence binds `batch` exactly |
| `#n` | `n` or 1 (parameters only). Like `n`, it binds `n` where it first appears |
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
(torch can't infer `-1` if they multiply to 0), and a callee's inferred requires. Relations, like a
kernel fitting the image, must be proved. See [docs/13-free-functions.md](../docs/13-free-functions.md).

Bodies must be straight-line code, apart from `if`s that are decided statically and loops over an
`nn.ModuleList`:

- `y = expr` and `y: Float[Tensor, "..."] = expr`. The annotation is checked, and it can bind new names.
- `a, b = expr`, unpacking a tuple.
- `return expr`, including `return a, b`.
- `if` on whether variables are `None` (`if mask is not None:`), which is known statically. See
  [Optional parameters](#optional-parameters).
- `assert`: the rest of the body assumes it. Comparisons of ints with `+ - * //` are stated, and so is
  `a % b == 0`, as `b * (a // b) == a`. A call in one, like `x.size(1)`, is evaluated first. Anything else
  in an assert is dropped, which is sound. A divisor may be inferred positive, like a size.
- `for layer in self.layers:` over an `nn.ModuleList`. The body is checked once, and the locals it
  reassigns must keep their shapes. See [Modules](#modules).
- `x[a:b] = v` assigns into a slice. `v` must broadcast to the slice, and `x` keeps its shape.
- `pass` is skipped.
- Expressions: local variables, int/bool/float literals, calls, `+ - * / // ** @ & | ^`, unary `-` and
  `~`, comparisons, methods (`x.sum(-1)`, `x.size(-1)`), properties (`x.mT`), tuples, and tuples of ints
  as shapes (`x.reshape((n, d))`). One entry of a shape may be `-1` where the stub determines it, as in
  `x.reshape(-1, d)`.
- Slices `x[a:b:c, ...]` of the leading dims, with Python's rules: negative bounds count from the end,
  and bounds past the end are clamped. So `x[:, :n]` has `min(n, m)` columns, which is `n` after
  `assert n <= m`. The step must be positive.

Anything else gets an explicit error, and the rest of the file is still checked. That covers other control
flow, augmented assignment (`x += y`), int indexing (`x[0]`) and `x.shape`, lambdas, and module-level
values.

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
decided the same way. A function may have at most 4 `Optional` parameters (16 cases). They aren't
supported on `__init__` or in stubs. See [docs/15-attention.md](../docs/15-attention.md).

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
- An attribute must be assigned once, at the top level of `__init__`. It can't be assigned anywhere
  else, and `__init__` can't reassign an int parameter.
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

Modules can't be returned or stored in locals yet, and an annotation can't name a module parameter's
dims. Only direct subclasses of `nn.Module` are checked. See [docs/14-modules.md](../docs/14-modules.md)
and [docs/16-stacks.md](../docs/16-stacks.md).

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
`nn.Linear`, `nn.Embedding`, `nn.LayerNorm`, `nn.Dropout` and `nn.ReLU`, `math.sqrt`/`math.log`, and
Python's operators. `operator.setitem(target, value)` is slice assignment: the frontend passes the slice
itself as `target`. `nn.ModuleList` has no stub; the frontend handles it. A call with no stub is an error,
never an unknown shape.

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
constr ["Eq"|"Le"|"Lt", entry, entry]
sig    {"params": [[name, typ]], "ret": typ, "requires": [constr], "exists": [name], "ensures": [constr],
        "invariant": [constr], "instances": [{"init": f, "ints": [[init_param, param]]}]}
term   ["Var", x] ["Lit", i] ["Call", f, [term]] ["Shape", [term]] ["Scalar"] ["Tuple", [term]]
       ["Slice", term, [[start, stop, step]]]            start, stop, step are terms or null
stmt   ["Let", x, term] ["LetAnnot", x, typ, term] ["Unpack", [x], term] ["Return", term]
       ["Assume", constr] ["Loop", [[x_before, x_after]], [stmt]] ["At", line, text, stmt]

program {"env": [{"name": f, "overloads": [sig]}],
         "functions": [{"name": f, "sig": sig, "body": [stmt] | null}]}
```

`None` is `["Tuple", []]`. A signature's `invariant` and `instances` are optional. An invariant is
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
`attention[mask=None]`.

A function whose body couldn't be translated has `"body": null`, so callers still use its signature. The
checker prints `{"results": [{"name": f, "error": null | message, "inferred": ["n >= 0", ...]}]}` and
exits 0 if everything checks, 1 if something doesn't, and 2 for invalid IR. `inferred` lists the
preconditions added to `f`'s signature.
