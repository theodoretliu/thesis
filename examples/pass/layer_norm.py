import torch
from jaxtyping import Float
from torch import Tensor


def layer_norm(
    x: Float[Tensor, "*batch d"], gamma: Float[Tensor, "d"], beta: Float[Tensor, "d"]
) -> Float[Tensor, "*batch d"]:
    mu = x.mean(-1, keepdim=True)
    var = ((x - mu) ** 2).mean(dim=-1, keepdim=True)
    return (x - mu) / torch.sqrt(var + 1e-5) * gamma + beta


def pooled(x: Float[Tensor, "b t d"]) -> Float[Tensor, "b d"]:
    return x.mean(1)
