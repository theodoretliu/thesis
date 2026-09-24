from jaxtyping import Float
from torch import Tensor


# expect-error: expected d, got t
def pooled(x: Float[Tensor, "b t d"]) -> Float[Tensor, "b d"]:
    return x.mean(-1)
