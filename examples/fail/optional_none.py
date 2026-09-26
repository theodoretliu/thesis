from typing import Optional

from jaxtyping import Bool, Float
from torch import Tensor


# each Optional parameter is checked as None too, where it can't be used
# expect-error: in masked[mask=None]: `mask` is None here
def masked(
    scores: Float[Tensor, "q k"], mask: Optional[Bool[Tensor, "q k"]] = None
) -> Float[Tensor, "q k"]:
    return scores.masked_fill(~mask, -1e9).softmax(dim=-1)
