import torch.nn as nn
from jaxtyping import Float
from torch import Tensor


class QK(nn.Module):
    def __init__(self, d: int):
        super().__init__()
        self.qkv = nn.Linear(d, 3 * d)

    def forward(self, x: Float[Tensor, "n d"]) -> Float[Tensor, "n n"]:
        # the projection is three pieces of size d, not two
        q, k = self.qkv(x).split(x.size(1), dim=1)
        return q @ k.transpose(0, 1)


# expect-error: in QK.forward, `q, k = self.qkv(x).split(x.size(1), dim=1)`
# expect-error: array of shape [n, d], array of shape [n, d], array of shape
