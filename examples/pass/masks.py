"""Masked attention: Optional masks and modules, asserts, and transposes.

A function with Optional parameters is checked once for each choice of which
are None, and `if mask is not None:` is decided in each. `*#b` broadcasts to
`*b`, and `#q` is q or 1, as in jaxtyping.
"""

import math
from typing import Optional

import torch.nn as nn
from jaxtyping import Bool, Float
from torch import Tensor


def masked_softmax(
    scores: Float[Tensor, "*b q k"],
    mask: Optional[Bool[Tensor, "*#b #q k"]] = None,
    dropout: Optional[nn.Dropout] = None,
) -> Float[Tensor, "*b q k"]:
    """A softmax over the keys the mask allows."""
    if mask is not None:
        scores = scores.masked_fill(~mask, -1e9)
    weights = scores.softmax(dim=-1)
    if dropout is not None:
        weights = dropout(weights)
    return weights


class SelfAttention(nn.Module):
    """h heads of d_model // h dims each. The assert is part of the class
    invariant, so forward can prove that h * d_k = d_model."""

    def __init__(self, d_model: int, h: int, dropout: float = 0.1):
        super().__init__()
        assert d_model % h == 0
        self.h = h
        self.d_k = d_model // h
        self.proj = nn.Linear(d_model, d_model)
        self.out = nn.Linear(d_model, d_model)
        self.dropout = nn.Dropout(dropout)

    def forward(
        self, x: Float[Tensor, "b n d_model"], mask: Optional[Bool[Tensor, "b #n n"]] = None
    ) -> Float[Tensor, "b n d_model"]:
        b = x.size(0)
        # [b, h, n, d_k]
        heads = self.proj(x).view(b, -1, self.h, self.d_k).transpose(1, 2)
        scores = heads @ heads.transpose(-2, -1) / math.sqrt(self.d_k)
        if mask is not None:
            mask = mask.unsqueeze(1)  # the same mask for every head
        weights = masked_softmax(scores, mask, dropout=self.dropout)
        merged = (weights @ heads).transpose(1, 2).reshape(b, -1, self.h * self.d_k)
        return self.out(merged)


def fold(x: Float[Tensor, "b n"], k: int) -> Float[Tensor, "b k n//k"]:
    """An assert in a function: the -1 is n // k."""
    assert x.size(1) % k == 0
    return x.reshape(x.size(0), k, -1)
