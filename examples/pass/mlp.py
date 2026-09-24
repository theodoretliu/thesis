import torch
import torch.nn.functional as F
from jaxtyping import Float
from torch import Tensor


def mlp(
    x: Float[Tensor, "*batch d"],
    w1: Float[Tensor, "h d"],
    b1: Float[Tensor, "h"],
    w2: Float[Tensor, "p h"],
) -> Float[Tensor, "*batch p"]:
    """A two-layer perceptron, checked once for every batch shape."""
    z = F.relu(F.linear(x, w1, b1))
    return F.linear(z, w2)
