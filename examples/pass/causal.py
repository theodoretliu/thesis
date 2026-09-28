"""Causal self-attention as nanoGPT writes it. x.size() unpacks into the
dims, split cuts the fused projection into q, k and v, and the mask buffer
exists only without the fast path, so forward reads it only in that case.
The mask is sliced to [:T, :T], and broadcasting it against the scores needs
T <= max_len: the checker infers that as a requires, which Block passes on
and GPTish proves from its assert."""

import math

import torch
import torch.nn as nn
from jaxtyping import Float
from torch import Tensor
from torch.nn import functional as F


class CausalSelfAttention(nn.Module):
    def __init__(self, d: int, n_head: int, max_len: int):
        super().__init__()
        assert d % n_head == 0
        self.qkv = nn.Linear(d, 3 * d)
        self.proj = nn.Linear(d, d)
        self.n_head = n_head
        self.p = 0.1
        self.fast = hasattr(F, "scaled_dot_product_attention")
        if not self.fast:
            self.register_buffer(
                "mask", torch.tril(torch.ones(max_len, max_len)).view(1, 1, max_len, max_len)
            )

    def forward(self, x: Float[Tensor, "b n d"]) -> Float[Tensor, "b n d"]:
        B, T, C = x.size()
        q, k, v = self.qkv(x).split(C, dim=2)
        q = q.view(B, T, self.n_head, C // self.n_head).transpose(1, 2)
        k = k.view(B, T, self.n_head, C // self.n_head).transpose(1, 2)
        v = v.view(B, T, self.n_head, C // self.n_head).transpose(1, 2)
        if self.fast:
            y = F.scaled_dot_product_attention(
                q, k, v, attn_mask=None, dropout_p=self.p if self.training else 0, is_causal=True
            )
        else:
            att = (q @ k.transpose(-2, -1)) * (1.0 / math.sqrt(k.size(-1)))
            att = att.masked_fill(self.mask[:, :, :T, :T] == 0, float("-inf"))
            y = F.softmax(att, dim=-1) @ v
        y = y.transpose(1, 2).contiguous().view(B, T, C)
        return self.proj(y)


class GLU(nn.Module):
    """A gated unit: the projection splits into two halves."""

    def __init__(self, d: int):
        super().__init__()
        self.up = nn.Linear(d, 2 * d)

    def forward(self, x: Float[Tensor, "*b d"]) -> Float[Tensor, "*b d"]:
        a, gate = self.up(x).split(x.size(-1), dim=-1)
        return a * torch.sigmoid(gate)


class Block(nn.Module):
    def __init__(self, d: int, n_head: int, max_len: int):
        super().__init__()
        self.attn = CausalSelfAttention(d, n_head, max_len)
        self.glu = GLU(d)

    def forward(self, x: Float[Tensor, "b n d"]) -> Float[Tensor, "b n d"]:
        x = x + self.attn(x)
        return x + self.glu(x)


class GPTish(nn.Module):
    def __init__(self, d: int, n_head: int, max_len: int):
        super().__init__()
        self.max_len = max_len
        self.block = Block(d, n_head, max_len)

    def forward(self, x: Float[Tensor, "b n d"]) -> Float[Tensor, "b n d"]:
        assert x.size(1) <= self.max_len
        return self.block(x)
