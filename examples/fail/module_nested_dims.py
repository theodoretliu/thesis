import torch.nn as nn
from jaxtyping import Float
from torch import Tensor


class Project(nn.Module):
    def __init__(self, d_in: int, d_out: int):
        super().__init__()
        self.w = nn.Linear(d_in, d_out)

    def forward(self, x: Float[Tensor, "b d_in"]) -> Float[Tensor, "b d_out"]:
        return self.w(x)


class Encoder(nn.Module):
    def __init__(self, d: int, e: int):
        super().__init__()
        # built for inputs of size e, but forward passes d
        self.proj = Project(e, d)

    # expect-error: in Encoder.forward, `return self.proj(x)`: Project.forward: Could not type check: parameter x (array[b, d_in], given array of shape [b, d]): expected d_in = e, got d
    def forward(self, x: Float[Tensor, "b d"]) -> Float[Tensor, "b d"]:
        return self.proj(x)
