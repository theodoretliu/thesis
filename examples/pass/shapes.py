import torch
from typing import Literal
from jaxtyping import Float, Int
from torch import Tensor


def split_heads(x: Float[Tensor, "8 12 64"]) -> Float[Tensor, "8 12 4 16"]:
    return x.reshape(8, 12, 4, 16)


def grid(k: Literal[3]) -> Float[Tensor, "4 4"]:
    n = k + 1
    return torch.zeros((n, n))


def dedupe(x: Int[Tensor, "n"]) -> Int[Tensor, "m"]:
    # m is existential: only the return type names it
    return torch.unique(x)


def scale(x: Float[Tensor, "*a"], factor: float = 2.0) -> Float[Tensor, "*a"]:
    return x * factor


def rescaled(x: Float[Tensor, "n d"]) -> Float[Tensor, "n d"]:
    # keyword and default arguments of a user function
    return scale(x, factor=0.5) + scale(x)


def transposed(x: Float[Tensor, "*a m n"]) -> Float[Tensor, "*a n m"]:
    return x.transpose(-1, -2)
