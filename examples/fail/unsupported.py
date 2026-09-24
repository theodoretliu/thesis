import torch
from jaxtyping import Float
from torch import Tensor

# the frontend rejects what it can't translate, and still checks the rest
# expect-error: unsupported.py:14: in branch: `if` isn't supported yet
# expect-error: unsupported.py:24: in untyped_call: `helper` has no annotations
# expect-error: unsupported.py:28: in unknown: no stub for `torch.frobnicate`
# expect-error: unsupported.py:32: in index: `x.shape[i]` isn't supported yet
# expect-error: unsupported.py:36: in mismatched, `return x * y`: operator.mul: No overload matches


def branch(x: Float[Tensor, "n"]) -> Float[Tensor, "n"]:
    if x.sum() > 0:
        return x
    return -x


def helper(x):
    return x


def untyped_call(x: Float[Tensor, "n"]) -> Float[Tensor, "n"]:
    return helper(x)


def unknown(x: Float[Tensor, "n"]) -> Float[Tensor, "n"]:
    return torch.frobnicate(x)


def index(x: Float[Tensor, "n d"]) -> Float[Tensor, "n"]:
    return torch.zeros(x.shape[0])


def mismatched(x: Float[Tensor, "n"], y: Float[Tensor, "m"]) -> Float[Tensor, "n"]:
    return x * y
