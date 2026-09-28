"""A small language model built the way nanoGPT's GPT is. Its modules are
in an nn.ModuleDict, and the output projection is tied to the token
embedding through it. Positions are made on the input's device. With targets,
forward returns logits for every position and a loss; without, it returns
logits for the last position only (x[:, [-1], :]) and None for the loss. So
the logits' length is "#t", meaning t or 1, and the loss is Optional. A caller
that takes the last position (logits[:, -1, :]) needs that length to be at
least 1, which holds when t is."""

from dataclasses import dataclass
from typing import Optional

import torch
import torch.nn as nn
from jaxtyping import Float, Int
from torch import Tensor
from torch.nn import functional as F


@dataclass
class LMConfig:
    block_size: int = 16
    vocab_size: int = 32
    n_layer: int = 2
    n_embd: int = 8
    dropout: float = 0.0


class MLPBlock(nn.Module):
    def __init__(self, config: LMConfig):
        super().__init__()
        self.ln = nn.LayerNorm(config.n_embd)
        self.up = nn.Linear(config.n_embd, 4 * config.n_embd)
        self.down = nn.Linear(4 * config.n_embd, config.n_embd)

    def forward(self, x: Float[Tensor, "b t n_embd"]) -> Float[Tensor, "b t n_embd"]:
        return x + self.down(F.gelu(self.up(self.ln(x))))


class TinyLM(nn.Module):
    def __init__(self, config: LMConfig):
        super().__init__()
        self.config = config
        self.transformer = nn.ModuleDict(
            dict(
                wte=nn.Embedding(config.vocab_size, config.n_embd),
                wpe=nn.Embedding(config.block_size, config.n_embd),
                drop=nn.Dropout(config.dropout),
                h=nn.ModuleList([MLPBlock(config) for _ in range(config.n_layer)]),
                ln_f=nn.LayerNorm(config.n_embd),
            )
        )
        self.lm_head = nn.Linear(config.n_embd, config.vocab_size, bias=False)
        self.transformer.wte.weight = self.lm_head.weight

    def hidden(self, idx: Int[Tensor, "b t"]) -> Float[Tensor, "b t n_embd"]:
        device = idx.device
        b, t = idx.size()
        assert t <= self.config.block_size
        pos = torch.arange(0, t, dtype=torch.long, device=device)
        x = self.transformer.drop(self.transformer.wte(idx) + self.transformer.wpe(pos))
        for block in self.transformer.h:
            x = block(x)
        return self.transformer.ln_f(x)

    def forward(
        self, idx: Int[Tensor, "b t"], targets: Optional[Int[Tensor, "b t"]] = None
    ) -> tuple[Float[Tensor, "b #t vocab_size"], Optional[Float[Tensor, ""]]]:
        x = self.hidden(idx)
        if targets is not None:
            logits = self.lm_head(x)
            loss = F.cross_entropy(
                logits.view(-1, logits.size(-1)), targets.view(-1), ignore_index=-1
            )
        else:
            logits = self.lm_head(x[:, [-1], :])
            loss = None
        return logits, loss

    def next_token_logits(self, idx: Int[Tensor, "b t"]) -> Float[Tensor, "b vocab_size"]:
        logits, _ = self(idx)
        return logits[:, -1, :]

    def first_state(self, idx: Int[Tensor, "b t"]) -> Float[Tensor, "b n_embd"]:
        return self.hidden(idx)[:, 0]
