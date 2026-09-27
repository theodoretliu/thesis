import torch
import torch.nn as nn
from jaxtyping import Float
from torch import Tensor


class Affine(nn.Module):
    def __init__(self, d: int, bias: bool):
        super().__init__()
        self.weight = nn.Parameter(torch.ones(d))
        self.bias = nn.Parameter(torch.zeros(d)) if bias else None

    def forward(self, x: Float[Tensor, "*b d"]) -> Float[Tensor, "*b d"]:
        # without the flag, there's no bias to add
        return x * self.weight + self.bias


# expect-error: in Affine.forward[bias=False]: `self.bias` is None here
