from typing import Optional

import torch.nn as nn
from jaxtyping import Float, Int
from torch import Tensor


class LM(nn.Module):
    def __init__(self, vocab: int, d: int):
        super().__init__()
        self.embed = nn.Embedding(vocab, d)
        self.head = nn.Linear(d, vocab)

    def forward(
        self, idx: Int[Tensor, "b t"], targets: Optional[Int[Tensor, "b t"]] = None
    ) -> tuple[Float[Tensor, "b #t vocab"], Optional[Float[Tensor, ""]]]:
        x = self.embed(idx)
        if targets is None:
            # x[:, -1, :] drops the time dim; nanoGPT keeps it with x[:, [-1], :]
            return self.head(x[:, -1, :]), None
        return self.head(x), None


# expect-error: in LM.forward[targets=None], `return self.head(x[:, -1, :]), None`
# expect-error: return value[0] (array[b, #t, vocab], given array of shape [b, vocab]): expected t or 1, got vocab
