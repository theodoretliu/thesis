"""Modules built from a config, as nanoGPT's are. The config's int fields are
the instance dims: forward's "*b d_model" is config.d_model."""

from dataclasses import dataclass

import torch
import torch.nn as nn
from jaxtyping import Float, Int
from torch import Tensor


class FeedForward(nn.Module):
    def __init__(self, config: "Config"):
        super().__init__()
        self.up = nn.Linear(config.d_model, config.expansion * config.d_model, bias=config.bias)
        self.act = nn.GELU()
        self.down = nn.Linear(config.expansion * config.d_model, config.d_model, bias=config.bias)
        self.dropout = nn.Dropout(config.dropout)

    def forward(self, x: Float[Tensor, "*b d_model"]) -> Float[Tensor, "*b d_model"]:
        return self.dropout(self.down(self.act(self.up(x))))


class Tagger(nn.Module):
    """Embed tokens, run a residual feed-forward block, and score each tag.
    nn.Linear requires out_features >= 0, so every Tagger has n_tags >= 0,
    and blank can build its zeros without requiring it."""

    def __init__(self, config: "Config"):
        super().__init__()
        self.config = config
        self.embed = nn.Embedding(config.vocab, config.d_model)
        self.norm = nn.LayerNorm(config.d_model)
        self.ff = FeedForward(config)
        self.head = nn.Linear(config.d_model, config.n_tags)

    def forward(self, tokens: Int[Tensor, "b n"]) -> Float[Tensor, "b n n_tags"]:
        x = self.embed(tokens)
        x = x + self.ff(self.norm(x))
        return self.head(x)

    def blank(self, tokens: Int[Tensor, "b n"]) -> Float[Tensor, "b n n_tags"]:
        return torch.zeros(tokens.size(0), tokens.size(1), self.config.n_tags)


@dataclass
class Config:
    vocab: int = 100
    d_model: int = 32
    expansion: int = 4
    n_tags: int = 5
    dropout: float = 0.1
    bias: bool = True
