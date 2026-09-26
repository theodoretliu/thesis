import torch.nn as nn
from jaxtyping import Float
from torch import Tensor


class Heads(nn.Module):
    # without `assert d_model % h == 0`, h heads of d_model // h dims may not
    # fill d_model, and d_model // h isn't known when h may be 0 or negative
    def __init__(self, d_model: int, h: int):
        super().__init__()
        self.h = h
        self.d_k = d_model // h
        self.proj = nn.Linear(d_model, d_model)

    # expect-error: in Heads.forward
    # expect-error: can't infer the size -1: the other sizes' product b * h * ? may be 0
    def forward(self, x: Float[Tensor, "b n d_model"]) -> Float[Tensor, "b h n d_model//h"]:
        return self.proj(x).view(x.size(0), -1, self.h, self.d_k).transpose(1, 2)
