import torch
import torch.nn as nn
from jaxtyping import Float
from torch import Tensor
from torch.nn import functional as F


class Masked(nn.Module):
    def __init__(self, max_len: int, d: int):
        super().__init__()
        self.fast = hasattr(F, "scaled_dot_product_attention")
        self.proj = nn.Linear(d, d)
        if not self.fast:
            # the mask is max_len x max_len, not max_len x d
            self.register_buffer(
                "mask", torch.tril(torch.ones(max_len, d)).view(1, max_len, max_len)
            )

    def forward(self, x: Float[Tensor, "b n d"]) -> Float[Tensor, "b n d"]:
        return self.proj(x)


# expect-error: in Masked.__init__[self.fast=False]
# expect-error: torch.Tensor.view
