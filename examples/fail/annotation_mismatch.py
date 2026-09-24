from jaxtyping import Float
from torch import Tensor


# an annotated assignment is checked too
# expect-error: annotation_mismatch.py:9:
# expect-error: annotation of h (array[b, d], given array of shape [b, p]): expected d, got p
def project(x: Float[Tensor, "b d"], w: Float[Tensor, "d p"]) -> Float[Tensor, "b d"]:
    h: Float[Tensor, "b d"] = x @ w
    return h
