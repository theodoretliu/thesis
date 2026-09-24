from jaxtyping import Float
from torch import Tensor


# the scores are [*batch, n, m]: the values were never applied
# expect-error: return value (array[*batch, n, e], given array of shape [*batch, n, m])
def attention(
    q: Float[Tensor, "*batch n d"], k: Float[Tensor, "*batch m d"], v: Float[Tensor, "*batch m e"]
) -> Float[Tensor, "*batch n e"]:
    scores = q @ k.mT
    return scores.softmax(-1)
