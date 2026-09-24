import torch.nn.functional as F
from jaxtyping import Float
from torch import Tensor


# F.linear takes weight as [out, in]
# expect-error: parameter weight (array[p, k], given array of shape [d, h]): expected k = d, got h
def mlp(x: Float[Tensor, "*batch d"], w: Float[Tensor, "d h"]) -> Float[Tensor, "*batch h"]:
    return F.relu(F.linear(x, w))
