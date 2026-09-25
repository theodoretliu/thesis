import math

import torch.nn as nn
from jaxtyping import Float, Int
from torch import Tensor


class FeedForward(nn.Module):
    """A pre-norm residual feed-forward block. d_model in forward's
    annotations is the d_model the block was built with."""

    def __init__(self, d_model: int, expansion: int = 4, dropout: float = 0.1):
        super().__init__()
        self.hidden = expansion * d_model
        self.norm = nn.LayerNorm(d_model)
        self.up = nn.Linear(d_model, self.hidden)
        self.down = nn.Linear(self.hidden, d_model)
        self.dropout = nn.Dropout(dropout)

    def forward(self, x: Float[Tensor, "*batch d_model"]) -> Float[Tensor, "*batch d_model"]:
        return x + self.dropout(self.down(self.up(self.norm(x)).relu()))


class Classifier(nn.Module):
    """Embed tokens, run a block, and score each position. n_classes isn't in
    forward's input shapes, but nn.Linear requires out_features >= 0, so
    every Classifier's self.head has a valid output size."""

    def __init__(self, vocab: int, d_model: int, n_classes: int):
        super().__init__()
        self.embed = nn.Embedding(vocab, d_model)
        self.block = FeedForward(d_model)
        self.head = nn.Linear(d_model, n_classes)
        self.scale = d_model

    def features(self, tokens: Int[Tensor, "b n"]) -> Float[Tensor, "b n d_model"]:
        return self.block(self.embed(tokens) * math.sqrt(self.scale))

    def forward(self, tokens: Int[Tensor, "b n"]) -> Float[Tensor, "b n n_classes"]:
        return self.head(self.features(tokens))
