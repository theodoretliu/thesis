from typing import Optional

import torch.nn as nn
from jaxtyping import Float, Int
from torch import Tensor
from torch.nn import functional as F


class LM(nn.Module):
    def __init__(self, vocab: int, d: int):
        super().__init__()
        self.embed = nn.Embedding(vocab, d)
        self.head = nn.Linear(d, vocab)

    def forward(
        self, idx: Int[Tensor, "b t"], targets: Optional[Int[Tensor, "b t"]] = None
    ) -> tuple[Float[Tensor, "b t vocab"], Optional[Float[Tensor, ""]]]:
        logits = self.head(self.embed(idx))
        if targets is None:
            return logits, None
        return logits, F.cross_entropy(logits.view(-1, logits.size(-1)), targets.view(-1))


class Trainer(nn.Module):
    def __init__(self, vocab: int, d: int):
        super().__init__()
        self.lm = LM(vocab, d)

    def scaled_loss(self, idx: Int[Tensor, "b t"]) -> Float[Tensor, ""]:
        # without targets, the loss is None
        _, loss = self.lm(idx)
        return loss * 2


# expect-error: in Trainer.scaled_loss, `return loss * 2`
# expect-error: given None or array of shape []
