"""Stacks of layers: nn.ModuleList loops, declared attributes, weight tying,
and slices.

A loop over an nn.ModuleList is checked once: every layer is built alike,
and the body must keep the shapes of the locals it reassigns. A class-level
annotation declares an attribute's shape over the instance dims, and every
assignment to it is checked, including register_buffer and a tied weight.
"""

import torch
import torch.nn as nn
from jaxtyping import Float, Int
from torch import Tensor


def downsample(x: Float[Tensor, "b n d"]) -> Float[Tensor, "b (n+1)//2 d"]:
    """Every other position, starting with the first."""
    return x[:, ::2]


class Residual(nn.Module):
    """x + W2 relu(W1 x)."""

    def __init__(self, d: int, hidden: int):
        super().__init__()
        self.w_1 = nn.Linear(d, hidden)
        self.w_2 = nn.Linear(hidden, d)

    def forward(self, x: Float[Tensor, "*b d"]) -> Float[Tensor, "*b d"]:
        return x + self.w_2(self.w_1(x).relu())


class Sinusoids(nn.Module):
    """Fixed position features: sines in the even columns, cosines in the odd
    ones, added to the first n rows."""

    table: Float[Tensor, "max_len d"]

    def __init__(self, d: int, max_len: int):
        super().__init__()
        assert d % 2 == 0  # as many sines as cosines
        table = torch.zeros(max_len, d)
        angles = torch.arange(0, max_len).float().unsqueeze(1) / torch.arange(1, d + 1, 2)
        table[:, 0::2] = torch.sin(angles)
        table[:, 1::2] = torch.cos(angles)
        self.register_buffer("table", table)

    def forward(self, x: Float[Tensor, "b n d"]) -> Float[Tensor, "b n d"]:
        assert x.size(1) <= self.table.size(0)
        return x + self.table[: x.size(1)]


class TiedStack(nn.Module):
    """A language model: embeddings, position features and n_layers residual
    blocks, with the output projection tied to the embedding."""

    def __init__(self, vocab: int, n_layers: int, d: int, max_len: int):
        super().__init__()
        self.embed = nn.Embedding(vocab, d)
        self.positions = Sinusoids(d, max_len)
        self.blocks = nn.ModuleList([Residual(d, 4 * d) for _ in range(n_layers)])
        self.out = nn.Linear(d, vocab, bias=False)
        self.out.weight = self.embed.weight

    def forward(self, tokens: Int[Tensor, "b n"]) -> Float[Tensor, "b n vocab"]:
        x = self.positions(self.embed(tokens))
        for block in self.blocks:
            x = block(x)
        return self.out(x)
