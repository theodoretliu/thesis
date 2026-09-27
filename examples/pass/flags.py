"""Modules whose attributes depend on flags, as nanoGPT's do. A flag is a
bool parameter of __init__, or an attribute __init__ sets to one or to
hasattr(...). A method is checked once for each value of the flags it
tests, and every case has the method's one signature."""

import torch
import torch.nn as nn
from jaxtyping import Float
from torch import Tensor
from torch.nn import functional as F


class LayerNorm(nn.Module):
    """nanoGPT's: without the flag, bias is None, and so is layer_norm's."""

    def __init__(self, ndim: int, bias: bool):
        super().__init__()
        self.weight = nn.Parameter(torch.ones(ndim))
        self.bias = nn.Parameter(torch.zeros(ndim)) if bias else None

    def forward(self, input: Float[Tensor, "*b ndim"]) -> Float[Tensor, "*b ndim"]:
        return F.layer_norm(input, self.weight.shape, self.weight, self.bias, 1e-5)


class Scale(nn.Module):
    """A gain, and a shift only with the flag. use_shift is the flag shift,
    so the cases of __init__ and forward are shift=True and shift=False."""

    def __init__(self, d: int, shift: bool = False):
        super().__init__()
        self.use_shift = shift
        self.gain = nn.Parameter(torch.ones(d))
        self.shift = nn.Parameter(torch.zeros(d)) if self.use_shift else None

    def forward(self, x: Float[Tensor, "*b d"]) -> Float[Tensor, "*b d"]:
        y = x * self.gain
        if self.shift is not None:
            y = y + self.shift
        return y


class Masked(nn.Module):
    """A causal mask, built only without the fast path, as nanoGPT's
    attention does. hasattr's value is only known when it runs, so __init__
    is checked for both. Only the slow case needs max_len >= 0, but callers
    can't pick a case, so the constructor requires it."""

    def __init__(self, max_len: int, d: int):
        super().__init__()
        self.fast = hasattr(F, "scaled_dot_product_attention")
        self.proj = nn.Linear(d, d)
        if not self.fast:
            print(f"WARNING: building a {max_len} x {max_len} mask")
            self.register_buffer("mask", torch.tril(torch.ones(max_len, max_len)))

    def forward(self, x: Float[Tensor, "b n d"]) -> Float[Tensor, "b n d"]:
        return self.proj(x)


class Block(nn.Module):
    """Passes its flag on without testing it, so it has no cases."""

    def __init__(self, d: int, max_len: int, bias: bool):
        super().__init__()
        self.norm = LayerNorm(d, bias)
        self.mix = Masked(max_len, d)
        self.scale = Scale(d, shift=bias)

    def forward(self, x: Float[Tensor, "b n d"]) -> Float[Tensor, "b n d"]:
        return x + self.scale(self.mix(self.norm(x)))
