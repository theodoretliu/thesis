import torch.nn.functional as F
from jaxtyping import Float
from torch import Tensor


# for a 0x0 image the 3x3 kernel doesn't fit, even padded by 1
# expect-error: Precondition not provable: kh = 3 <= h + (2 * padding) = h + 2
def strided(
    x: Float[Tensor, "n c h w"], k: Float[Tensor, "o c 3 3"]
) -> Float[Tensor, "n o (h-1)//2+1 (w-1)//2+1"]:
    return F.conv2d(x, k, stride=2, padding=1)
