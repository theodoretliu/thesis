import torch
from jaxtyping import Int
from torch import Tensor


# unique may drop elements: only m <= n is known
# expect-error: return value (array[n], given array of shape
def dedupe(x: Int[Tensor, "n"]) -> Int[Tensor, "n"]:
    return torch.unique(x)
