import torch
import torch.nn as nn
from jaxtyping import Float
from torch import Tensor


class Masked(nn.Module):
    def __init__(self, max_len: int):
        super().__init__()
        self.register_buffer("mask", torch.tril(torch.ones(max_len, max_len)))

    def forward(self, x: Float[Tensor, "n n"]) -> Float[Tensor, "n n"]:
        T = x.size(0)
        return x.masked_fill(self.mask[:T, :T] == 0, float("-inf"))


class Model(nn.Module):
    def __init__(self):
        super().__init__()
        self.masked = Masked(8)

    def forward(self, x: Float[Tensor, "16 16"]) -> Float[Tensor, "16 16"]:
        # the mask covers 8 positions, so Masked.forward requires n <= 8
        return self.masked(x)


# expect-error: in Model.forward, `return self.masked(x)`
# expect-error: Precondition not provable: n = 16 <= max_len = 8
