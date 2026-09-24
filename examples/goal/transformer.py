"""The Transformer from "Attention Is All You Need" (Vaswani et al., 2017).

This is the checker's goal: every annotated function and method here should
check. frontend/tests/test_goal.py tracks which ones do, and
docs/12-transformer-goal.md lists what's missing.

The code is plain PyTorch with jaxtyping annotations, written the way a user
would write it. It follows the paper: post-norm residual blocks (section 3.1),
scaled dot-product and multi-head attention (3.2), position-wise feed-forward
layers (3.3), embeddings scaled by sqrt(d_model) and tied to the output
projection (3.4), and sinusoidal positional encodings (3.5). It's
runtime-correct: examples/goal/run_transformer.py runs it under jaxtyping's
runtime checker.

Dims: b batch, s source length, t target length, q and m query and key
lengths. A dim named after a constructor's int (d_model, d_ff, h, d_k, vocab,
max_len) is that int.
"""

import math
from typing import Optional

import torch
import torch.nn as nn
import torch.nn.functional as F
from jaxtyping import Bool, Float, Int
from torch import Tensor


def attention(
    query: Float[Tensor, "*b q d_k"],
    key: Float[Tensor, "*b m d_k"],
    value: Float[Tensor, "*b m d_v"],
    mask: Optional[Bool[Tensor, "*#b #q m"]] = None,
    dropout: Optional[nn.Dropout] = None,
) -> Float[Tensor, "*b q d_v"]:
    """Scaled dot-product attention (section 3.2.1)."""
    d_k = query.size(-1)
    scores = query @ key.transpose(-2, -1) / math.sqrt(d_k)
    if mask is not None:
        scores = scores.masked_fill(mask == 0, -1e9)
    p_attn = scores.softmax(dim=-1)
    if dropout is not None:
        p_attn = dropout(p_attn)
    return p_attn @ value


def subsequent_mask(size: int) -> Bool[Tensor, "1 size size"]:
    """Hide later positions from each position of the decoder (section 3.2.3)."""
    return torch.triu(torch.ones(1, size, size), diagonal=1) == 0


def make_masks(
    src: Int[Tensor, "b s"], tgt: Int[Tensor, "b t"], pad: int
) -> tuple[Bool[Tensor, "b 1 s"], Bool[Tensor, "b t t"]]:
    """Hide padding in the source, and padding and the future in the target."""
    src_mask = (src != pad).unsqueeze(-2)
    tgt_mask = (tgt != pad).unsqueeze(-2) & subsequent_mask(tgt.size(-1))
    return src_mask, tgt_mask


class MultiHeadedAttention(nn.Module):
    """h attention heads over d_k = d_model / h dims each (section 3.2.2)."""

    def __init__(self, h: int, d_model: int, dropout: float = 0.1):
        super().__init__()
        assert d_model % h == 0
        self.h = h
        self.d_k = d_model // h
        self.w_q = nn.Linear(d_model, d_model)
        self.w_k = nn.Linear(d_model, d_model)
        self.w_v = nn.Linear(d_model, d_model)
        self.w_o = nn.Linear(d_model, d_model)
        self.dropout = nn.Dropout(p=dropout)

    def forward(
        self,
        query: Float[Tensor, "b q d_model"],
        key: Float[Tensor, "b m d_model"],
        value: Float[Tensor, "b m d_model"],
        mask: Optional[Bool[Tensor, "b #q m"]] = None,
    ) -> Float[Tensor, "b q d_model"]:
        if mask is not None:
            mask = mask.unsqueeze(1)  # the same mask for every head
        nbatches = query.size(0)
        q = self.w_q(query).view(nbatches, -1, self.h, self.d_k).transpose(1, 2)
        k = self.w_k(key).view(nbatches, -1, self.h, self.d_k).transpose(1, 2)
        v = self.w_v(value).view(nbatches, -1, self.h, self.d_k).transpose(1, 2)
        x = attention(q, k, v, mask=mask, dropout=self.dropout)
        x = x.transpose(1, 2).contiguous().view(nbatches, -1, self.h * self.d_k)
        return self.w_o(x)


class PositionwiseFeedForward(nn.Module):
    """FFN(x) = max(0, x W1 + b1) W2 + b2 (section 3.3)."""

    def __init__(self, d_model: int, d_ff: int, dropout: float = 0.1):
        super().__init__()
        self.w_1 = nn.Linear(d_model, d_ff)
        self.w_2 = nn.Linear(d_ff, d_model)
        self.dropout = nn.Dropout(dropout)

    def forward(self, x: Float[Tensor, "b n d_model"]) -> Float[Tensor, "b n d_model"]:
        return self.w_2(self.dropout(self.w_1(x).relu()))


class Embeddings(nn.Module):
    """Token embeddings, scaled by sqrt(d_model) (section 3.4)."""

    def __init__(self, d_model: int, vocab: int):
        super().__init__()
        self.lut = nn.Embedding(vocab, d_model)
        self.d_model = d_model

    def forward(self, x: Int[Tensor, "b n"]) -> Float[Tensor, "b n d_model"]:
        return self.lut(x) * math.sqrt(self.d_model)


class PositionalEncoding(nn.Module):
    """Sinusoidal positional encodings, added to the embeddings (section 3.5)."""

    pe: Float[Tensor, "1 max_len d_model"]

    def __init__(self, d_model: int, dropout: float, max_len: int = 5000):
        super().__init__()
        assert d_model % 2 == 0  # sin and cos take half the dims each
        self.dropout = nn.Dropout(p=dropout)
        pe = torch.zeros(max_len, d_model)
        position = torch.arange(0, max_len).unsqueeze(1)
        div_term = torch.exp(torch.arange(0, d_model, 2) * -(math.log(10000.0) / d_model))
        pe[:, 0::2] = torch.sin(position * div_term)
        pe[:, 1::2] = torch.cos(position * div_term)
        self.register_buffer("pe", pe.unsqueeze(0))

    def forward(self, x: Float[Tensor, "b n d_model"]) -> Float[Tensor, "b n d_model"]:
        assert x.size(1) <= self.pe.size(1)  # no encoding past max_len
        return self.dropout(x + self.pe[:, : x.size(1)])


class EncoderLayer(nn.Module):
    """Self-attention, then a feed-forward layer, each wrapped as
    LayerNorm(x + Dropout(Sublayer(x))) (sections 3.1 and 5.4)."""

    def __init__(self, d_model: int, d_ff: int, h: int, dropout: float):
        super().__init__()
        self.self_attn = MultiHeadedAttention(h, d_model, dropout)
        self.feed_forward = PositionwiseFeedForward(d_model, d_ff, dropout)
        self.norm1 = nn.LayerNorm(d_model)
        self.norm2 = nn.LayerNorm(d_model)
        self.dropout = nn.Dropout(dropout)

    def forward(
        self, x: Float[Tensor, "b s d_model"], mask: Bool[Tensor, "b 1 s"]
    ) -> Float[Tensor, "b s d_model"]:
        x = self.norm1(x + self.dropout(self.self_attn(x, x, x, mask)))
        return self.norm2(x + self.dropout(self.feed_forward(x)))


class DecoderLayer(nn.Module):
    """Masked self-attention, attention over the encoder's output, then a
    feed-forward layer (section 3.1)."""

    def __init__(self, d_model: int, d_ff: int, h: int, dropout: float):
        super().__init__()
        self.self_attn = MultiHeadedAttention(h, d_model, dropout)
        self.src_attn = MultiHeadedAttention(h, d_model, dropout)
        self.feed_forward = PositionwiseFeedForward(d_model, d_ff, dropout)
        self.norm1 = nn.LayerNorm(d_model)
        self.norm2 = nn.LayerNorm(d_model)
        self.norm3 = nn.LayerNorm(d_model)
        self.dropout = nn.Dropout(dropout)

    def forward(
        self,
        x: Float[Tensor, "b t d_model"],
        memory: Float[Tensor, "b s d_model"],
        src_mask: Bool[Tensor, "b 1 s"],
        tgt_mask: Bool[Tensor, "b t t"],
    ) -> Float[Tensor, "b t d_model"]:
        x = self.norm1(x + self.dropout(self.self_attn(x, x, x, tgt_mask)))
        x = self.norm2(x + self.dropout(self.src_attn(x, memory, memory, src_mask)))
        return self.norm3(x + self.dropout(self.feed_forward(x)))


class Encoder(nn.Module):
    """A stack of n_layers encoder layers."""

    def __init__(self, n_layers: int, d_model: int, d_ff: int, h: int, dropout: float):
        super().__init__()
        self.layers = nn.ModuleList(
            [EncoderLayer(d_model, d_ff, h, dropout) for _ in range(n_layers)]
        )

    def forward(
        self, x: Float[Tensor, "b s d_model"], mask: Bool[Tensor, "b 1 s"]
    ) -> Float[Tensor, "b s d_model"]:
        for layer in self.layers:
            x = layer(x, mask)
        return x


class Decoder(nn.Module):
    """A stack of n_layers decoder layers."""

    def __init__(self, n_layers: int, d_model: int, d_ff: int, h: int, dropout: float):
        super().__init__()
        self.layers = nn.ModuleList(
            [DecoderLayer(d_model, d_ff, h, dropout) for _ in range(n_layers)]
        )

    def forward(
        self,
        x: Float[Tensor, "b t d_model"],
        memory: Float[Tensor, "b s d_model"],
        src_mask: Bool[Tensor, "b 1 s"],
        tgt_mask: Bool[Tensor, "b t t"],
    ) -> Float[Tensor, "b t d_model"]:
        for layer in self.layers:
            x = layer(x, memory, src_mask, tgt_mask)
        return x


class Transformer(nn.Module):
    """The encoder-decoder model with a shared source/target vocabulary. The
    embeddings and the output projection share one weight (section 3.4)."""

    def __init__(
        self,
        vocab: int,
        n_layers: int = 6,
        d_model: int = 512,
        d_ff: int = 2048,
        h: int = 8,
        dropout: float = 0.1,
    ):
        super().__init__()
        self.embed = Embeddings(d_model, vocab)
        self.positions = PositionalEncoding(d_model, dropout)
        self.encoder = Encoder(n_layers, d_model, d_ff, h, dropout)
        self.decoder = Decoder(n_layers, d_model, d_ff, h, dropout)
        self.proj = nn.Linear(d_model, vocab, bias=False)
        self.proj.weight = self.embed.lut.weight

    def encode(
        self, src: Int[Tensor, "b s"], src_mask: Bool[Tensor, "b 1 s"]
    ) -> Float[Tensor, "b s d_model"]:
        return self.encoder(self.positions(self.embed(src)), src_mask)

    def decode(
        self,
        memory: Float[Tensor, "b s d_model"],
        src_mask: Bool[Tensor, "b 1 s"],
        tgt: Int[Tensor, "b t"],
        tgt_mask: Bool[Tensor, "b t t"],
    ) -> Float[Tensor, "b t d_model"]:
        return self.decoder(self.positions(self.embed(tgt)), memory, src_mask, tgt_mask)

    def forward(
        self,
        src: Int[Tensor, "b s"],
        tgt: Int[Tensor, "b t"],
        src_mask: Bool[Tensor, "b 1 s"],
        tgt_mask: Bool[Tensor, "b t t"],
    ) -> Float[Tensor, "b t vocab"]:
        """Logits for the next token at each target position."""
        return self.proj(self.decode(self.encode(src, src_mask), src_mask, tgt, tgt_mask))


def loss(
    logits: Float[Tensor, "b t vocab"], target: Int[Tensor, "b t"], pad: int
) -> Float[Tensor, ""]:
    """Cross-entropy with label smoothing 0.1, ignoring padding (section 5.4)."""
    return F.cross_entropy(
        logits.reshape(-1, logits.size(-1)),
        target.reshape(-1),
        ignore_index=pad,
        label_smoothing=0.1,
    )
