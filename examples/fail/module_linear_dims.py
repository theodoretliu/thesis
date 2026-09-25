import torch.nn as nn
from jaxtyping import Float
from torch import Tensor


class FeedForward(nn.Module):
    def __init__(self, d_model: int, d_ff: int):
        super().__init__()
        # nn.Linear takes (in_features, out_features)
        self.w_1 = nn.Linear(d_ff, d_model)
        self.w_2 = nn.Linear(d_ff, d_model)

    # expect-error: in FeedForward.forward, `return self.w_2(self.w_1(x).relu())`: torch.nn.Linear.forward
    # expect-error: parameter input (array[*B, in_features], given array of shape [b, n, d_model]): expected in_features = d_ff, got d_model
    def forward(self, x: Float[Tensor, "b n d_model"]) -> Float[Tensor, "b n d_model"]:
        return self.w_2(self.w_1(x).relu())
