from typing import Optional

from jaxtyping import Bool, Float
from torch import Tensor


def masked(
    scores: Float[Tensor, "*b q k"], mask: Optional[Bool[Tensor, "*#b #q k"]] = None
) -> Float[Tensor, "*b q k"]:
    if mask is not None:
        scores = scores.masked_fill(~mask, -1e9)
    return scores.softmax(dim=-1)


# the scores have a heads dim, so the [b, q, k] mask needs mask.unsqueeze(1)
# expect-error: in per_head, `return masked(scores, mask)`
# expect-error: [b] doesn't broadcast to *b = [b, h]
def per_head(
    scores: Float[Tensor, "b h q k"], mask: Bool[Tensor, "b q k"]
) -> Float[Tensor, "b h q k"]:
    return masked(scores, mask)
