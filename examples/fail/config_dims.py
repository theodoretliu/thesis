from dataclasses import dataclass

import torch.nn as nn
from jaxtyping import Float
from torch import Tensor


@dataclass
class Config:
    d_model: int
    d_ff: int


class FeedForward(nn.Module):
    def __init__(self, config: Config):
        super().__init__()
        self.up = nn.Linear(config.d_model, config.d_ff)
        # nn.Linear takes (in_features, out_features): these are swapped
        self.down = nn.Linear(config.d_model, config.d_ff)

    def forward(self, x: Float[Tensor, "b d_model"]) -> Float[Tensor, "b d_model"]:
        return self.down(self.up(x).relu())


# expect-error: in FeedForward.forward
# expect-error: expected in_features = d_model, got d_ff
