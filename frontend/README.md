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

Every top-level function with at least one annotation. Classes are skipped with a note. Every parameter
and the return type need annotations:

| Annotation | Checker type |
|---|---|
| `Float[Tensor, "*batch n d"]` (any jaxtyping dtype, any array type) | an array of that shape |
| `int`, `bool` | an int |
| `Literal[3]`, `Literal[True]` | that int (bools are 0 and 1) |
| `float` | a 0-d array: a float broadcasts like one |
| `Tensor`, `np.ndarray` with no shape | a parameter of any shape (not allowed as a return type) |
| `tuple[A, B]` (return types only) | a tuple of those types |

Shape strings follow jaxtyping:

| jaxtyping | Meaning |
|---|---|
| `b`, `seq`, `3` | a named or fixed dim |
| `*batch` | a named run of dims |
| `...`, `*_` | an unnamed run of dims (not in a return type) |
| `_`, `_foo` | a dim that isn't checked |
| `*#batch` | a run of dims that broadcasts with `batch`. The first occurrence binds `batch` exactly |
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

Bodies must be straight-line code:

- `y = expr` and `y: Float[Tensor, "..."] = expr`. The annotation is checked, and it can bind new names.
- `a, b = expr`, unpacking a tuple.
- `return expr`, including `return a, b`.
- `assert` and `pass` are skipped. Dropping a runtime check is sound.
- Expressions: local variables, int/bool/float literals, calls, `+ - * / // ** @ & | ^`, unary `-` and
  `~`, comparisons, methods (`x.sum(-1)`, `x.size(-1)`), properties (`x.mT`), tuples, and tuples of ints
  as shapes (`x.reshape((n, d))`). One entry of a shape may be `-1` where the stub determines it, as in
  `x.reshape(-1, d)`.

Anything else gets an explicit error, and the rest of the file is still checked. That covers control flow,
augmented assignment (`x += y`), indexing and `x.shape`, lambdas, and module-level values.

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
- List functions: `*drop(A,i)`, `*keep(A,i)`, `*permute(A,i,j)`, `*setat(A,i,d)`, `*insertat(A,i,d)`,
  `*broadcast(A,B)`. Inside arithmetic, `prod(A)`, `rank(A)`, and `A[i]` (one dim, e.g.
  `-> Dim["A[dim]"]` for `x.size(dim)`).
- `Dim["expr"]` is an int equal to a dim expression, e.g. `-> Dim["rank(A)"]` for `x.dim()`.
- `Shape["*S"]` is a tuple of ints used as a shape. As `*size: Shape["*S"]` it collects int arguments, so
  `torch.zeros(n, d)` and `torch.zeros((n, d))` both work. One entry may be `-1` if an equation the stub
  asserts determines it, like reshape's `assert prod(A) == prod(B)`: the other sizes must have a positive
  product that divides the total. Any other negative entry is an error, and a possibly negative one
  needs an inferred precondition.
- Asserts in the body are preconditions (`assert prod(A) == prod(B)`), or postconditions if they mention
  an existential (`assert m <= n` for `unique`).

The shipped stubs cover common torch functions, `Tensor` methods, `torch.nn.functional` and Python's
operators. A call with no stub is an error, never an unknown shape.

## The IR

The JSON mirrors the OCaml types in `checker/src/typing.ml`. Each value is a list tagged with its
constructor's name. `checker/bin/ir_json.ml` decodes it.

```
entry  ["Id", n] ["Int", i] ["Add"|"Sub"|"Mul"|"Div", e, e] ["Spread", A] ["Broadcast", A]
       ["Broadcasted", [A, ...]] ["Drop"|"Keep"|"Permute", A, [idx]] ["SetAt", A, [idx], e]
       ["InsertAt", A, idx, e] ["Prod", A] ["Rank", A] ["Index", A, idx]
                                                                 idx is a name or an int
typ    ["Array", [entry]] ["Int"] ["IntExpr", entry] ["Literal", i] ["Tuple", [typ]]
constr ["Eq"|"Le"|"Lt", entry, entry]
sig    {"params": [[name, typ]], "ret": typ, "requires": [constr], "exists": [name], "ensures": [constr]}
term   ["Var", x] ["Lit", i] ["Call", f, [term]] ["Shape", [term]] ["Scalar"] ["Tuple", [term]]
stmt   ["Let", x, term] ["LetAnnot", x, typ, term] ["Unpack", [x], term] ["Return", term]
       ["At", line, text, stmt]

program {"env": [{"name": f, "overloads": [sig]}],
         "functions": [{"name": f, "sig": sig, "body": [stmt] | null}]}
```

A function whose body couldn't be translated has `"body": null`, so callers still use its signature. The
checker prints `{"results": [{"name": f, "error": null | message, "inferred": ["n >= 0", ...]}]}` and
exits 0 if everything checks, 1 if something doesn't, and 2 for invalid IR. `inferred` lists the
preconditions added to `f`'s signature.
