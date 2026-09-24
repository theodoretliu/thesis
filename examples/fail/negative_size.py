import torch
from jaxtyping import Float
from torch import Tensor


# n is any int, and torch.zeros(-1) fails
# expect-error: shape entry n' may be negative
def zeros(n: int) -> Float[Tensor, "n"]:
    return torch.zeros(n)
