import torch
from jaxtyping import Float
from torch import Tensor


def attention(
    q: Float[Tensor, "*batch n d"],
    k: Float[Tensor, "*batch m d"],
    v: Float[Tensor, "*batch m e"],
) -> Float[Tensor, "*batch n e"]:
    scores = q @ k.mT / 8.0
    weights = scores.softmax(dim=-1)
    return weights @ v


def self_attention(
    x: Float[Tensor, "*batch n d"],
    wq: Float[Tensor, "d d"],
    wk: Float[Tensor, "d d"],
    wv: Float[Tensor, "d d"],
) -> Float[Tensor, "*batch n d"]:
    # a call to a function defined above, through its signature
    return attention(x @ wq, x @ wk, x @ wv)
