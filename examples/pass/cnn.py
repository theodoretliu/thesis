import torch
import torch.nn.functional as F
from jaxtyping import Float
from torch import Tensor


def classify(
    images: Float[Tensor, "b 3 32 32"],
    kernel: Float[Tensor, "16 3 5 5"],
    bias: Float[Tensor, "16"],
    w: Float[Tensor, "10 4096"],
) -> Float[Tensor, "b 10"]:
    h = F.relu(F.conv2d(images, kernel, bias, padding=2))  # b 16 32 32
    h = F.max_pool2d(h, 2)  # b 16 16 16
    return F.linear(h.flatten(1), w)

