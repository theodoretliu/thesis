import torch
from jaxtyping import Float
from torch import Tensor


# torch.zeros(-1) raises, so zeros requires n >= 0, inferred from its body
def zeros(n: int) -> Float[Tensor, "n"]:
    return torch.zeros(n)


# and k - 2 may be negative
# expect-error: in shrink, `return zeros(k - 2)`: zeros: Precondition not provable: n = k - 2 >= 0
def shrink(k: int) -> Float[Tensor, "k-2"]:
    return zeros(k - 2)
