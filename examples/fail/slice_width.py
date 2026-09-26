import torch
import torch.nn as nn
from jaxtyping import Float
from torch import Tensor


# without `assert d % 2 == 0`, the odd columns are one fewer than the even
# ones when d is odd
# expect-error: in Sinusoids.__init__, `table[:, 1::2] = torch.cos(angles)`
# expect-error: [max_len, (d + 1) // 2] doesn't broadcast to *A = [max_len, d[1::2]]
class Sinusoids(nn.Module):
    table: Float[Tensor, "max_len d"]

    def __init__(self, d: int, max_len: int):
        super().__init__()
        table = torch.zeros(max_len, d)
        angles = torch.arange(0, max_len).float().unsqueeze(1) / torch.arange(1, d + 1, 2)
        table[:, 0::2] = torch.sin(angles)
        table[:, 1::2] = torch.cos(angles)
        self.register_buffer("table", table)
