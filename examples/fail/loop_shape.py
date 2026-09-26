import torch.nn as nn
from jaxtyping import Float
from torch import Tensor


class Widen(nn.Module):
    def __init__(self, d: int):
        super().__init__()
        self.w = nn.Linear(d, 2 * d)

    def forward(self, x: Float[Tensor, "b d"]) -> Float[Tensor, "b 2*d"]:
        return self.w(x)


# each layer doubles the width, so the second one can't take the first's output
# expect-error: in Stack.forward, `for layer in self.layers: ...`
# expect-error: `x` doesn't keep its shape in the loop: array of shape [b, d] before the body, array of shape [b, 2 * d] after
class Stack(nn.Module):
    def __init__(self, n: int, d: int):
        super().__init__()
        self.layers = nn.ModuleList([Widen(d) for _ in range(n)])

    def forward(self, x: Float[Tensor, "b d"]) -> Float[Tensor, "b d"]:
        for layer in self.layers:
            x = layer(x)
        return x
