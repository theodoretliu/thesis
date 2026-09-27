import torch
import torch.nn as nn
from jaxtyping import Float
from torch import Tensor
from torch.nn import functional as F


class Masked(nn.Module):
    def __init__(self, max_len: int):
        super().__init__()
        self.fast = hasattr(F, "scaled_dot_product_attention")
        if not self.fast:
            self.register_buffer("mask", torch.tril(torch.ones(max_len, max_len)))

    def forward(self, x: Float[Tensor, "n n"]) -> Float[Tensor, "n n"]:
        # the mask only exists without the fast path
        if self.fast:
            return x.masked_fill(self.mask[: x.size(0), : x.size(0)] == 0, float("-inf"))
        return x


# expect-error: in Masked.forward[self.fast=True]: `self.mask` isn't assigned in this case
